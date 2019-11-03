open System
[<Literal>]
let EventLockFileName = "lockedevents.hash"

open System.Reflection
let matchingTypesInAssembly (predicate: Type -> bool) (asm: Assembly) =
    let filter = Array.filter (fun (t: Type) -> (isNull t |> not) && t.Assembly = asm && predicate t)
    try
        asm.GetTypes() |> filter
    with
        | :? ReflectionTypeLoadException as ex -> ex.Types |> filter

let getAllTypes asm = asm |> matchingTypesInAssembly (fun _ -> true)

let getMarkerType (marker: string) allTypes = allTypes |> Array.tryFind (fun (t: Type) -> t.FullName.EndsWith(marker))

let loadAssemblyTypes assemblyPath = assemblyPath |> Assembly.LoadFile |> getAllTypes

let getMarkerTypeFromAssembly assemblyTypes = 
    match getMarkerType "IEvent" assemblyTypes with
    | Some(t) -> t
    | None -> failwith "Unable to find marker type 'IEvent'"

let getTypesUsingMarker (markerType: Type) = Array.filter (fun t -> markerType.IsAssignableFrom(t) && markerType <> t)

open Microsoft.FSharp.Reflection
// Taken from https://github.com/dmannock/FSharpUnionHelpers
type PublicTypeSignature =
    | SimpleTypeSig of TypeName: string
    | ClassTypeSig of TypeName: string * Fields: Fields list
    | RecordTypeSig of TypeName: string * Fields: Fields list
    | UnionTypeSig of TypeName: string * Unions: Fields list
    // | TupleTypeSig of Fields: Fields list
    // | EnumTypeSig of TypeName: string * Fields: string list
    | UnsupportedTypeSig of Type
and Fields = {
    Identifier: string
    TypeSignature: PublicTypeSignature
}
let rec getTypesPublicSignature (t: Type) = 
    let bindingFlags = BindingFlags.Public ||| BindingFlags.Instance
    let propertiesToPublicSignature = 
        Seq.map (fun (pi: PropertyInfo) -> {
            Identifier = pi.Name
            TypeSignature = getTypesPublicSignature pi.PropertyType
        })
        >> List.ofSeq
    if t.IsPrimitive then SimpleTypeSig(t.Name)
    else if t = typeof<String> then SimpleTypeSig(t.Name)
    else if t = typeof<DateTime> then SimpleTypeSig(t.Name)
    else if FSharpType.IsRecord t then 
        let fields = FSharpType.GetRecordFields t |> propertiesToPublicSignature
        RecordTypeSig(t.Name, fields) 
    else if FSharpType.IsUnion t then 
        let ucInfo (uc: UnionCaseInfo) = uc.GetFields() |> Seq.map (fun i -> {
            Identifier = uc.Name
            TypeSignature = getTypesPublicSignature i.PropertyType
        })
        let unions = 
            FSharpType.GetUnionCases(t)
            |> Seq.map (ucInfo)
            |> Seq.collect id
            |> List.ofSeq
        UnionTypeSig(t.Name, unions)                        
    else if t.IsClass then
        let properties = t.GetProperties(bindingFlags) |> propertiesToPublicSignature
        let fields = 
            t.GetFields(bindingFlags)
            |> Seq.map (fun fi -> {
                Identifier = fi.Name
                TypeSignature = getTypesPublicSignature fi.FieldType
            })
            |> List.ofSeq
        ClassTypeSig(t.Name, properties@fields)  
    else
        UnsupportedTypeSig(t)        
let rec toSignatureString signature =
    let fieldToString { Identifier = ident; TypeSignature = typeSig } = sprintf "%s:%s" ident (toSignatureString typeSig)
    match signature with
    | SimpleTypeSig(typeName) -> typeName
    | ClassTypeSig(typeName, fields) -> sprintf "%s={%s}" typeName (String.Join(";", fields |> List.map fieldToString))
    | RecordTypeSig(typeName, fields) -> sprintf "%s={%s}" typeName (String.Join(";", fields |> List.map fieldToString))                     
    | UnionTypeSig(typeName, unions) -> sprintf "%s=%s" typeName (String.Join(";", unions |> List.map (fieldToString >> sprintf "|%s")))
    | UnsupportedTypeSig(t) -> failwithf "UNSUPPORTED TYPE SIGNATURE: %A" t
//end of borrowed FSharpUnionHelpers code

type EventHash =  {
    Type: string 
    Hash: string 
}
let createEventHash t h = {Type = t; Hash = h}

// placeholder fn signature for primitive types currently
let typeToEventHash (hashFn: string -> string) (t: Type) =
    let hashType theType =
        theType
        |> getTypesPublicSignature
        |> toSignatureString
        |> hashFn
    createEventHash t.Name (hashType  t)
    |> List.singleton 

let getEventHashesForAssembly hashFn assemblyPath =
    let assemblyTypes = loadAssemblyTypes assemblyPath
    let markerType = getMarkerTypeFromAssembly assemblyTypes
    getTypesUsingMarker markerType assemblyTypes
    |> List.ofArray
    |> List.collect (typeToEventHash hashFn)
    |> List.distinct

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
    if IO.File.Exists file then
        IO.File.ReadAllLines file
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

let checkEventHashesForDifferences =
    List.choose (
        function 
        | SameEventSignature(_) -> None
        | NewEventSignature(newEvent) -> Some (sprintf "New Event: %A" newEvent)
        | DeletedEventSignature(deletedEvent) -> Some (sprintf "Deleted Event: %A" deletedEvent)
        | EventSignatureChanged(orig, changed) -> Some (sprintf "Changed Event '%s'. Original hash: %s Current hash: %s" orig.Type orig.Hash changed.Hash)
    )
    >> function
    | [] -> Ok ()
    | errors -> Error errors

let runBuildHashComparison assemblyPath hashLockFilePath =
    let originalEventHashes = 
        match readEventHashesFromFile hashLockFilePath with
        | Some(hashes) -> hashes
        | None -> failwithf "No event lock file found at %s\nTo start using event locking generate the initial lock by runing with the '--addnew' argument." hashLockFilePath
    getEventHashesForAssembly hashSha assemblyPath
    |> compareEventHash originalEventHashes    
    |> checkEventHashesForDifferences
    |> Result.map (fun _ -> sprintf "Event checks complete. %i Events have not been mutated" (List.length originalEventHashes))
    |> Result.mapError (fun errors -> sprintf "Errors in event checks:\n%s" (String.Join("\n", errors)))

let runAddNewHashes assemblyPath hashLockFilePath =
    let originalEventHashes = readEventHashesFromFile hashLockFilePath |> Option.defaultValue List.Empty
    let eventComparisons = 
        getEventHashesForAssembly hashSha assemblyPath
        |> compareEventHash originalEventHashes
    let mutatedEvents = eventComparisons |> List.choose (function 
            | EventSignatureChanged(orig, changed) -> Some (sprintf "Changed Event '%s'. Original hash: %s Current hash: %s" orig.Type orig.Hash changed.Hash)
            | DeletedEventSignature(deletedEvent) -> Some (sprintf "Deleted Event %A" deletedEvent)
            | _ -> None)
    let newEvents = eventComparisons |> List.choose (function 
            | NewEventSignature(event) -> Some event
            | _ -> None)
    match mutatedEvents, newEvents with
    | [], [] -> Ok <| sprintf "No new events to add to the existing %i events." (List.length originalEventHashes)
    | [], newEvents ->
        let origLookup = originalEventHashes |> eventListToMap
        let originalWithNewEvents = 
            newEvents 
            |> List.fold (fun acc cur -> Map.add cur.Type cur acc) origLookup
            |> Map.toList
            |> List.map snd
        saveEventHashesToFile hashLockFilePath originalWithNewEvents
        Ok <| sprintf "%i Event hashes added the the original %i. New total of %i" (List.length newEvents) (List.length originalEventHashes) (List.length originalWithNewEvents)
    | mutatedEvents, _ -> Error <| sprintf "Cannot update event hashes. Events have been mutated:\n%s" (String.Join("\n", mutatedEvents))

// command line running
type RunMode = CompareEvents | ForceEventUpdates
type CommandLineOptions = {
    AssemblyPath: string
    HashLockFilePath: string
    RunMode: RunMode
}
let parseArgs() =
    match fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray with
    | assemblyPath::hashLockFilePath::"--addnew"::_ -> {
            AssemblyPath = assemblyPath
            HashLockFilePath = IO.Path.Combine(hashLockFilePath, EventLockFileName)
            RunMode = ForceEventUpdates
        }
    | assemblyPath::hashLockFilePath::_ -> {
            AssemblyPath = assemblyPath
            HashLockFilePath = IO.Path.Combine(hashLockFilePath, EventLockFileName)
            RunMode = CompareEvents
        }
    | _ -> failwith """Assemply path must be passed as the first argument. HashLockFilePath as the second.
                    To generate initial event locks or add new events run with the '--addnew' argument."""

let run commandLineOptions =
    printfn "\n##########################################################################\n"
    printfn "Running event locker with options:\n %A" commandLineOptions
    match commandLineOptions with
    | { RunMode = ForceEventUpdates } as cmd -> runAddNewHashes cmd.AssemblyPath cmd.HashLockFilePath
    | { RunMode = CompareEvents } as cmd-> runBuildHashComparison cmd.AssemblyPath cmd.HashLockFilePath

let printResult res = 
    printfn "\n##########################################################################\n"
    match res with
    | Ok(msg) -> printfn "✔️ %s" msg
    | Error(msg) -> 
        printfn "❌ %s" msg
        exit 1
// entry point
parseArgs()
|> run
|> printResult