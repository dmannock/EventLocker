open System
[<Literal>]
let EventLockFileName = "lockedevents.hash"

open System.Reflection
let tryLoadAssembly a =
    try
        a |> Assembly.LoadFile |> Some
    with
        | _ -> None    

open System.IO
let getAssemblyDirectory (a: Assembly) =
    let uri = UriBuilder(a.CodeBase)
    let path = Uri.UnescapeDataString uri.Path
    Path.GetDirectoryName path

let loadAssemblyWithDependencyResolution mainAssemblyPath =
    let currentDomainAssemblyResolve = new ResolveEventHandler(fun _ args ->
        let currentAssemblies = AppDomain.CurrentDomain.GetAssemblies()
        match currentAssemblies |> Array.tryFind (fun a -> a.FullName = args.Name) with
        | Some(resolvedAssembly) -> resolvedAssembly
        | None ->
            let assemblyDir = args.RequestingAssembly |> getAssemblyDirectory
            let filename = (args.Name.Split(',').[0]) + ".dll".ToLower()
            Path.Combine(assemblyDir, filename)
            |> tryLoadAssembly
            |> Option.defaultValue null
    )
    AppDomain.CurrentDomain.add_AssemblyResolve currentDomainAssemblyResolve
    mainAssemblyPath |> Assembly.LoadFile

let getAllTypes (asm: Assembly) =
    let filter = Array.filter (fun (t: Type) -> (isNull t |> not) && t.Assembly = asm)
    try
        asm.GetTypes() |> filter
    with
        | :? ReflectionTypeLoadException as ex -> ex.Types |> filter

let getTypesSubclassing (markerType: Type) = 
    List.filter (fun t -> markerType <> t && not t.IsAbstract && (markerType.IsAssignableFrom t || t.IsSubclassOf markerType))

let getChildTypes markerType types =
    let rec loop parent remaining found =
        match remaining with
        | x::xs -> loop x xs found@(getTypesSubclassing parent remaining)
        | _ -> []    
    loop markerType types []

let getMarkerType (marker: string) allTypes = allTypes |> List.tryFind (fun (t: Type) -> t.FullName.EndsWith(marker))

let getMarkerTypeFromAssembly markerType assemblyTypes = 
    match getMarkerType markerType assemblyTypes with
    | Some(t) -> t
    | None -> failwithf "Unable to find marker type '%s'" markerType

open Microsoft.FSharp.Reflection
// Taken from https://github.com/dmannock/FSharpUnionHelpers
type PublicTypeSignature =
    | SimpleTypeSig of TypeName: string
    | ClassTypeSig of TypeName: string * Fields: Fields list
    | RecordTypeSig of TypeName: string * Fields: Fields list
    | UnionTypeSig of TypeName: string * Unions: Fields list
    | TupleTypeSig of Fields: PublicTypeSignature list
    | EnumTypeSig of TypeName: string * FieldTypeName: string * Fields: string list
and Fields = {
    Identifier: string
    TypeSignature: PublicTypeSignature
}
let createTypeSigFields id tsig = { Identifier = id; TypeSignature = tsig }

let getUnionCases t = FSharpType.GetUnionCases(t, BindingFlags.NonPublic ||| BindingFlags.Public)
let isUnion t = FSharpType.IsUnion(t, BindingFlags.NonPublic ||| BindingFlags.Instance)

let rec getTypesPublicSignature (t: Type) = 
    let publicBindingFlags = BindingFlags.Public ||| BindingFlags.Instance
    let propertiesToPublicSignature = 
        Seq.map (fun (pi: PropertyInfo) -> createTypeSigFields pi.Name (getTypesPublicSignature pi.PropertyType))
        >> Seq.sortBy (fun x -> x.Identifier)
        >> List.ofSeq
    if FSharpType.IsRecord t then 
        RecordTypeSig(t.Name, (FSharpType.GetRecordFields t |> propertiesToPublicSignature)) 
    else if FSharpType.IsTuple t then 
        TupleTypeSig(FSharpType.GetTupleElements t |> Array.map getTypesPublicSignature |> List.ofArray)        
    else if t.IsEnum then 
        EnumTypeSig(t.Name, Enum.GetUnderlyingType(t).ToString(), Enum.GetNames(t) |> List.ofArray)        
    else if isUnion t && not (typeof<Collections.IEnumerable>.IsAssignableFrom(t)) then 
        let ucInfo (uc: UnionCaseInfo) = 
            uc.GetFields() 
            |> Seq.map (fun i -> createTypeSigFields uc.Name (getTypesPublicSignature i.PropertyType))
        let unions = 
            getUnionCases t
            |> Seq.sortBy (fun uc -> uc.Name)
            |> Seq.map (ucInfo)
            |> Seq.collect id
            |> List.ofSeq
        UnionTypeSig(t.Name, unions) 
    else if t.IsClass && t <> typeof<String> && not (typeof<Collections.IEnumerable>.IsAssignableFrom(t)) then
        let properties = t.GetProperties(publicBindingFlags) |> propertiesToPublicSignature
        let fields = 
            t.GetFields(publicBindingFlags)
            |> Seq.map (fun fi -> createTypeSigFields fi.Name (getTypesPublicSignature fi.FieldType))
            |> Seq.sortBy (fun x -> x.Identifier)
            |> List.ofSeq
        ClassTypeSig(t.ToString(), properties@fields)  
    else   
        SimpleTypeSig(t.ToString())  

let rec toSignatureString signature =
    let fieldToString { Identifier = ident; TypeSignature = typeSig } = sprintf "%s:%s" ident (toSignatureString typeSig)
    match signature with
    | SimpleTypeSig(typeName) -> typeName
    | ClassTypeSig(typeName, fields) -> sprintf "%s={%s}" typeName (String.Join("#", fields |> List.map fieldToString))
    | RecordTypeSig(typeName, fields) -> sprintf "%s={%s}" typeName (String.Join(";", fields |> List.map fieldToString))                     
    | UnionTypeSig(typeName, unions) -> sprintf "%s=%s" typeName (String.Join("", unions |> List.map (fieldToString >> sprintf "|%s")))
    | TupleTypeSig(fields) -> sprintf "(%s)" (String.Join(",", fields |> List.map toSignatureString))
    | EnumTypeSig(typeName, fieldTypeName, fields) -> sprintf "%s:%s={%s}" typeName fieldTypeName (String.Join(",", fields))  
//end of borrowed FSharpUnionHelpers code

let groupUnionCaseEvents =
    function
    // filter out Unions where the union type & case type implement the marker type
    | UnionTypeSig(typeName, unions) as evt -> 
        match unions |> List.tryFind (fun u -> u.Identifier = typeName) with
        | Some(_) -> List.empty
        | None -> 
            unions |> List.map (fun uc -> Some(uc.Identifier), UnionTypeSig(uc.Identifier, List.singleton uc))
    | evt -> List.singleton (None, evt)

type EventHash =  {
    Type: string 
    Hash: string 
}
let createEventHash t h = {Type = t; Hash = h}

let typeToEventHash (hashFn: string -> string) (t: Type) =
    getTypesPublicSignature t
    |> groupUnionCaseEvents
    |> List.map (fun x ->
        snd x
        |> toSignatureString 
        |> hashFn
        |> createEventHash (fst x |> Option.defaultValue t.Name))

let getEventHashesForAssembly markerType hashFn assemblyPath =
    let mainAssembly = assemblyPath |> loadAssemblyWithDependencyResolution
    let assemblyDir = mainAssembly |> getAssemblyDirectory
    let loadedDeps = 
        mainAssembly.GetReferencedAssemblies()
        |> Array.map (fun aName -> Path.Combine(assemblyDir, aName.Name))
        |> Array.choose tryLoadAssembly
        |> Array.distinct
        |> List.ofArray
    let assemblyTypes = 
        mainAssembly::loadedDeps
        |> List.collect (getAllTypes >> List.ofArray) 
    let markerType = getMarkerTypeFromAssembly markerType assemblyTypes
    getChildTypes markerType assemblyTypes
    |> List.collect (typeToEventHash hashFn)

//event hash lock file comparison
type EventComparison =
    | SameEventSignature of EventHash
    | NewEventSignature of EventHash
    | DeletedEventSignature of EventHash
    | EventSignatureChanged of Original: EventHash * Current: EventHash

let eventListToMap = List.map (fun x -> x.Type, x) >> Map.ofList

let compareEventHash originalHashLock currentHashes =
    let origEventsMap = originalHashLock |> eventListToMap
    let currentEventsMap = currentHashes |> eventListToMap
    [
        yield! currentHashes 
            |> Seq.map (fun current -> 
                match origEventsMap |> Map.tryFind current.Type with
                | Some(existing) when existing.Hash = current.Hash -> SameEventSignature(existing)
                | Some(existing) -> EventSignatureChanged(existing, current)
                | None -> NewEventSignature(current))
        yield! originalHashLock
            |> Seq.choose (fun orig -> 
                match currentEventsMap |> Map.tryFind orig.Type with
                | Some(_) -> None
                | None -> DeletedEventSignature(orig) |> Some
            )
    ]

let readEventHashesFromFile file =
    if File.Exists file then
        File.ReadAllLines file
        |> Array.choose (fun l -> 
            match l.Split(',') with
            | [|t; hash|] -> createEventHash t hash |> Some
            | _ -> None) 
        |> List.ofArray 
        |> Some |> Option.filter (List.isEmpty >> not)
    else None    

let saveEventHashesToFile file eventHashes =
    let lines = eventHashes |> Seq.map (fun { Type = t; Hash = hash} -> sprintf "%s,%s" t hash)
    IO.File.WriteAllLines(file, lines)

let hashSha (text: string) =
    if String.IsNullOrEmpty text then String.Empty
    else
        use sha = new Security.Cryptography.SHA256Managed()
        (System.Text.Encoding.UTF8.GetBytes text |> sha.ComputeHash |> BitConverter.ToString).Replace("-", String.Empty)

let runBuildHashComparison markerType assemblyPath hashLockFileLocation =
    match IO.Path.Combine(hashLockFileLocation, EventLockFileName) |> readEventHashesFromFile with
    | Some(originalEventHashes) -> 
        getEventHashesForAssembly markerType hashSha assemblyPath
        |> compareEventHash originalEventHashes
        |> List.choose (
            function 
            | SameEventSignature(_) -> None
            | NewEventSignature(newEvent) -> Some (sprintf "New Event: %A" newEvent)
            | DeletedEventSignature(deletedEvent) -> Some (sprintf "Deleted Event: %A" deletedEvent)
            | EventSignatureChanged(orig, changed) -> Some (sprintf "Changed Event '%s'. Original hash: %s Current hash: %s" orig.Type orig.Hash changed.Hash)
        )
        |> function
        | [] -> Ok (sprintf "Event checks complete. %i Events have not been mutated" (List.length originalEventHashes))
        | errors -> Error (sprintf "Errors in event checks:\n%s" (String.Join("\n", errors)))
    | None -> 
        Error (sprintf "No event lock file found. To start using event locking generate the initial lock by runing with the '--addnew' argument:\n\
            dotnet fsi EventLocker.fsx \"%s\" \"%s\" --addnew\n" assemblyPath hashLockFileLocation)

let runAddNewHashes markerType assemblyPath hashLockFilePath =
    let originalEventHashes = 
        readEventHashesFromFile hashLockFilePath
        |> Option.defaultValue List.Empty
    let eventComparisons = 
        getEventHashesForAssembly markerType hashSha assemblyPath
        |> compareEventHash originalEventHashes
    let mutatedEvents = eventComparisons |> List.choose (function 
            | EventSignatureChanged(orig, changed) -> Some (sprintf "Changed Event '%s'. Original hash: %s Current hash: %s" orig.Type orig.Hash changed.Hash)
            | DeletedEventSignature(deletedEvent) -> Some (sprintf "Deleted Event %A" deletedEvent)
            | _ -> None)
    let newEvents = eventComparisons |> List.choose (function 
            | NewEventSignature(event) -> Some event
            | _ -> None)
    let numOfOriginalEventHashes = List.length originalEventHashes  
    printfn "mutatedEvents events: %A" mutatedEvents   
    printfn "new events: %A" newEvents   
    match mutatedEvents, newEvents with
    | [], [] -> Ok <| sprintf "No new events to add to the existing %i events." numOfOriginalEventHashes
    | [], newEvents ->
        let origLookup = originalEventHashes |> eventListToMap
        let originalWithNewEvents = 
            newEvents 
            |> List.fold (fun acc cur -> Map.add cur.Type cur acc) origLookup
            |> Map.toList
            |> List.map snd
        saveEventHashesToFile hashLockFilePath originalWithNewEvents
        let numOfTotalEvents = List.length originalWithNewEvents
        Ok <| sprintf "%i Event hashes added the the original %i. New total of %i" (numOfTotalEvents - numOfOriginalEventHashes) numOfOriginalEventHashes numOfTotalEvents
    | mutatedEvents, _ -> Error <| sprintf "Cannot update event hashes. Events have been mutated:\n%s" (String.Join("\n", mutatedEvents))

// command line running
type RunMode = CompareEvents | ForceEventUpdates
type CommandLineOptions = {
    AssemblyPath: string
    HashLockFileLocation: string
    MarkerType: string
    RunMode: RunMode
}
with 
    static member Create(assemblyPath, hashLockFileLocation, markerType, runMode) = {
        AssemblyPath = assemblyPath
        HashLockFileLocation = hashLockFileLocation
        MarkerType = markerType |> Option.defaultValue "IEvent"
        RunMode = runMode
    }

let parseArgs(args) =
    match args |> Array.skip 1 with
    | [|assemblyPath;hashLockFileLocation;"--addnew"|]-> 
        CommandLineOptions.Create(assemblyPath, hashLockFileLocation, None, ForceEventUpdates)
    | [|assemblyPath;hashLockFileLocation;markerType;"--addnew"|] -> 
        CommandLineOptions.Create(assemblyPath, hashLockFileLocation, (Some markerType), ForceEventUpdates)
    | [|assemblyPath;hashLockFileLocation;markerType|] ->
        CommandLineOptions.Create(assemblyPath, hashLockFileLocation, (Some markerType), CompareEvents)
    | [|assemblyPath;hashLockFileLocation|] ->
        CommandLineOptions.Create(assemblyPath, hashLockFileLocation, None, CompareEvents)
    | _ -> failwith """Assembly path must be passed as the first argument. HashLockFileLocation as the second.
                    To generate initial event locks or add new events run with the '--addnew' argument."""

let run commandLineOptions =
    printfn "\n##########################################################################\n"
    printfn "Running event locker with options:\n %A" commandLineOptions
    match commandLineOptions with
    | { RunMode = ForceEventUpdates } as cmd -> runAddNewHashes cmd.MarkerType cmd.AssemblyPath (IO.Path.Combine(cmd.HashLockFileLocation, EventLockFileName))
    | { RunMode = CompareEvents } as cmd-> runBuildHashComparison cmd.MarkerType cmd.AssemblyPath cmd.HashLockFileLocation

let printResult res = 
    printfn "\n##########################################################################\n"
    match res with
    | Ok(msg) -> printfn "✔️ %s" msg
    | Error(msg) -> 
        printfn "❌ %s" msg
        exit 1
// entry point
parseArgs(fsi.CommandLineArgs)
|> run
|> printResult