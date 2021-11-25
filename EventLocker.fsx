//https://github.com/dmannock/EventLocker/tree/net6
open System
open System.Reflection
open System.IO

[<Literal>]
let EventLockFileName = "lockedevents.hash"

let getPackageFoldersFromPropsFile path =
    let regexMatch = System.Text.RegularExpressions.Regex.Match(File.ReadAllText path, "<NuGetPackageFolders.*?>(.*?)</NuGetPackageFolders>")
    if regexMatch.Success then
        regexMatch.Groups[1].Value.Split(';') |> Some
    else
        None

let tryLoadAssembly a =
    try
        a |> Assembly.LoadFile |> Some
    with _ -> None

open System.Runtime.InteropServices

let genPossibleAssemblyFilePaths
    (mainAssemblyFilePath: string)
    projectPath
    nugetPackagePaths
    (dependencyName: string)
    version
    frameworkVer
    =
    let dependencyNameForOs =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then
            dependencyName
        else
            dependencyName.ToLowerInvariant()

    let createFullNugetPathForVersion basePath =
        Path.Combine(basePath, dependencyNameForOs, version, "lib", frameworkVer)

    seq {
        yield Path.GetDirectoryName(mainAssemblyFilePath)
        yield Path.Combine(projectPath, "packages", "lib")

        yield!
            nugetPackagePaths
            |> List.map createFullNugetPathForVersion

        yield!
            nugetPackagePaths
            |> List.choose
                (fun nugetPath ->
                    Path.Combine(nugetPath, dependencyNameForOs)
                    |> Some
                    |> Option.filter Directory.Exists
                    |> Option.bind (
                        Directory.GetDirectories
                        >> Array.sort
                        >> Array.tryLast
                    ))
            |> List.map (fun latestPath -> Path.Combine(latestPath, "lib", frameworkVer))

        yield
            Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".nuget/packages")
            |> createFullNugetPathForVersion
    }

// quick filter to save loading assemblies we can avoid resolving
let canSkipDependencyLoading (dependencyName: string) =
    dependencyName.StartsWith("System.")
    || dependencyName.StartsWith("Microsoft.")
    || dependencyName = "FSharp.Core"

let tryLoadAssemblyForPaths genPossiblePathsForDependency (assemblyFullNameOrPath: String) =
    match assemblyFullNameOrPath.Split(',') with
    | [| dependencyName;             _; _; _ |] when AppDomain.CurrentDomain.GetAssemblies() |> Array.exists (fun a -> a.FullName = dependencyName) || canSkipDependencyLoading dependencyName -> None
    | [| dependencyName;    rawVersion; _; _ |] ->
        let version = rawVersion.Replace(" Version=", "")
        let filename = dependencyName + ".dll"

        [ "netstandard2.0"; "netstandard1.5"; "netstandard1.0" ]
        |> Seq.collect (genPossiblePathsForDependency dependencyName version)
        |> Seq.distinct
        |> Seq.map (fun p -> Path.Combine(p, filename))
        |> Seq.tryFind File.Exists
        |> Option.bind tryLoadAssembly
    | _ -> tryLoadAssembly assemblyFullNameOrPath

let getAssemblyDirectory (a: Assembly) =
    (UriBuilder a.Location).Path
    |> Uri.UnescapeDataString
    |> Path.GetDirectoryName

let loadAssemblyWithDependencyResolution assemblyLoader mainAssemblyFilePath =
    let currentDomainAssemblyResolve =
        new ResolveEventHandler(fun _ args ->
            if canSkipDependencyLoading args.Name then
                null
            else
                match AppDomain.CurrentDomain.GetAssemblies() |> Array.tryFind (fun a -> a.FullName = args.Name) with
                | Some (resolvedAssembly) -> resolvedAssembly
                | None ->
                    args.Name
                    |> assemblyLoader
                    |> Option.defaultValue null)

    AppDomain.CurrentDomain.add_AssemblyResolve currentDomainAssemblyResolve
    mainAssemblyFilePath |> tryLoadAssembly

let getAllTypes (asm: Assembly) =
    let filter = Array.filter (fun (t: Type) -> not (isNull t) && t.Assembly = asm)
    try
        asm.GetTypes() |> filter
    with :? ReflectionTypeLoadException as ex -> ex.Types |> filter

let getMarkerType (markerTypeName: string) allTypes =
    allTypes
    |> List.tryFind (fun (t: Type) -> t.FullName.EndsWith(markerTypeName))

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

let createTypeSigFields id tsig = { 
    Identifier = id
    TypeSignature = tsig
}

let getUnionCases t = FSharpType.GetUnionCases(t, BindingFlags.NonPublic ||| BindingFlags.Public)

let isUnion t = FSharpType.IsUnion(t, BindingFlags.NonPublic ||| BindingFlags.Instance)

let rec getTypesPublicSignature (t: Type) =
    let publicBindingFlags = BindingFlags.Public ||| BindingFlags.Instance
    let propertiesToPublicSignature =
        Seq.map (fun (pi: PropertyInfo) -> createTypeSigFields pi.Name (getTypesPublicSignature pi.PropertyType))
        >> Seq.sortBy (fun x -> x.Identifier)
        >> List.ofSeq
    if FSharpType.IsRecord t then
        RecordTypeSig(
            t.Name,
            (FSharpType.GetRecordFields t |> propertiesToPublicSignature)
        )
    else if FSharpType.IsTuple t then
        TupleTypeSig(
            FSharpType.GetTupleElements t
            |> Array.map getTypesPublicSignature
            |> List.ofArray
        )
    else if t.IsEnum then
        EnumTypeSig(t.Name, Enum.GetUnderlyingType(t).ToString(), Enum.GetNames(t) |> List.ofArray)
    else if
        isUnion t && not (typeof<Collections.IEnumerable>.IsAssignableFrom (t))
    then
        let ucInfo (uc: UnionCaseInfo) =
            uc.GetFields()
            |> Seq.map (fun i -> createTypeSigFields uc.Name (getTypesPublicSignature i.PropertyType))
        let unions =
            getUnionCases t
            |> Seq.sortBy (fun uc -> uc.Name)
            |> Seq.collect ucInfo
            |> List.ofSeq
        UnionTypeSig(t.Name, unions)
    else if
        t.IsClass && t <> typeof<String>
        && not (typeof<Collections.IEnumerable>.IsAssignableFrom t)
    then
        let properties =
            t.GetProperties(publicBindingFlags)
            |> propertiesToPublicSignature
        let fields =
            t.GetFields(publicBindingFlags)
            |> Seq.map (fun fi -> createTypeSigFields fi.Name (getTypesPublicSignature fi.FieldType))
            |> Seq.sortBy (fun x -> x.Identifier)
            |> List.ofSeq
        ClassTypeSig(t.ToString(), properties @ fields)
    else
        SimpleTypeSig(t.ToString())

let rec toSignatureString signature =
    let fieldToString { Identifier = ident; TypeSignature = typeSig } = $"{ident}:{toSignatureString typeSig}"
    match signature with
    | SimpleTypeSig (typeName) -> typeName
    | ClassTypeSig (typeName, fields) -> $"""{typeName}={{{ fields |> List.map fieldToString |> String.concat "#" }}}"""
    | RecordTypeSig (typeName, fields) -> $"""{typeName}={{{ fields |> List.map fieldToString |> String.concat ";" }}}"""
    | UnionTypeSig (typeName, unions) -> $"""{typeName}={unions |> List.fold (fun acc union -> acc + "|" + fieldToString union) ""}"""
    | TupleTypeSig (fields) -> $"""({fields |> List.map toSignatureString |> String.concat "," })"""
    | EnumTypeSig (typeName, fieldTypeName, fields) -> $"""{typeName}:{fieldTypeName}={{{String.concat "," fields}}}"""
//end of borrowed FSharpUnionHelpers code

let groupUnionCaseEvents =
    function
    // filter out Unions where the union type & case type implement the marker type
    | UnionTypeSig (typeName, unions) ->
        match unions |> List.tryFind (fun u -> u.Identifier = typeName) with
        | Some (_) -> List.empty
        | None -> unions |> List.map (fun uc -> Some(uc.Identifier), UnionTypeSig(uc.Identifier, List.singleton uc))
    | evt -> List.singleton (None, evt)

let isConcreteSubtypeOf (markerType: Type) (t: Type) =
    if isUnion t then
        markerType <> t
        && (markerType.IsAssignableFrom t
            || t.IsSubclassOf markerType)
    else
        markerType <> t
        && t.IsPublic
        && not t.IsAbstract
        && (markerType.IsAssignableFrom t
            || t.IsSubclassOf markerType)

type EventHash = { Type: string; Hash: string }
let createEventHash t h = { Type = t; Hash = h }

let typeToEventHash (hashFn: string -> string) (t: Type) =
    getTypesPublicSignature t
    |> groupUnionCaseEvents
    |> List.map
        (fun (name, signature) ->
            signature
            |> toSignatureString
            |> hashFn
            |> createEventHash (Option.defaultValue t.Name name))

let getEventHashesForAssembly markerTypeName hashFn assemblyFilePath projectPath =
    let nugetPackagePaths =
        Path.Combine(projectPath, "obj")
        |> Directory.GetFiles
        |> Seq.tryFind (fun f -> f.EndsWith("nuget.g.props"))
        |> Option.bind getPackageFoldersFromPropsFile
        |> Option.map List.ofArray
        |> Option.defaultWith (fun () -> 
            printfn "failed to get nuget package paths. attempting to continue without resolving those packages" 
            List.empty)

    let assemblyLoader =
        genPossibleAssemblyFilePaths assemblyFilePath projectPath nugetPackagePaths
        |> tryLoadAssemblyForPaths

    match assemblyFilePath |> loadAssemblyWithDependencyResolution assemblyLoader with
    | Some (loaded) ->
        let loadedDeps =
            loaded.GetReferencedAssemblies()
            |> Array.map (fun aName -> aName.FullName)
            |> Array.choose assemblyLoader
            |> Array.distinct
            |> List.ofArray

        let loadedAssemblies = loaded :: loadedDeps

        let assemblyTypes =
            loadedAssemblies
            |> List.collect (getAllTypes >> List.ofArray)

        let markerType =
            match getMarkerType markerTypeName assemblyTypes with
            | Some (t) -> t
            | None ->
                printfn "loaded assemblies:"
                loadedAssemblies |> List.iter (fun a -> printfn $"{a.FullName}")
                failwithf $"Unable to find marker type '{markerTypeName}' in {List.length loadedAssemblies} loaded assemblies with {List.length assemblyTypes} types"

        let concreteSubTypes = assemblyTypes |> List.filter (isConcreteSubtypeOf markerType)

        concreteSubTypes
        |> List.collect (typeToEventHash hashFn)
    | None -> failwith "failed loading main assembly"

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
        yield!
            currentHashes
            |> Seq.map
                (fun current ->
                    match Map.tryFind current.Type origEventsMap with
                    | Some (existing) when existing.Hash = current.Hash -> SameEventSignature(existing)
                    | Some (existing) -> EventSignatureChanged(existing, current)
                    | None -> NewEventSignature(current))
        yield!
            originalHashLock
            |> Seq.choose
                (fun orig ->
                    match Map.tryFind orig.Type currentEventsMap with
                    | Some (_) -> None
                    | None -> DeletedEventSignature(orig) |> Some) ]

let readEventHashesFromFile file =
    if File.Exists file then
        File.ReadAllLines file
        |> Array.choose
            (fun l ->
                match l.Split(',') with
                | [| t; hash |] -> createEventHash t hash |> Some
                | _ -> None)
        |> List.ofArray
        |> Some
        |> Option.filter (List.isEmpty >> not)
    else
        None

let saveEventHashesToFile file eventHashes =
    let lines =
        eventHashes
        |> Seq.map (fun { Type = t; Hash = hash } -> $"{t},{hash}")
    File.WriteAllLines(file, lines)

let hashSha (text: string) =
    if String.IsNullOrEmpty text then
        String.Empty
    else
        use sha = Security.Cryptography.SHA256.Create()
        (System.Text.Encoding.UTF8.GetBytes text |> sha.ComputeHash |> BitConverter.ToString).Replace("-", String.Empty)

let runBuildHashComparison markerType assemblyFilePath projectPath =
    match Path.Combine(projectPath, EventLockFileName) |> readEventHashesFromFile with
    | Some (originalEventHashes) ->
        getEventHashesForAssembly markerType hashSha assemblyFilePath projectPath
        |> compareEventHash originalEventHashes
        |> List.choose
            (function
            | SameEventSignature (_) -> None
            | NewEventSignature (newEvent) -> Some $"New Event: {newEvent}"
            | DeletedEventSignature (deletedEvent) -> Some $"Deleted Event: {deletedEvent}"
            | EventSignatureChanged (orig, changed) -> Some $"Changed Event '{orig.Type}'. Original hash: {orig.Hash} Current hash: {changed.Hash}")
        |> function
        | [] -> Ok $"Event checks complete. {List.length originalEventHashes} Events have not been mutated"
        | errors -> Error ($"{List.length errors} Errors in event checks:\n" + String.concat "\n" errors)
    | None ->
        Error $"No event lock file found. To start using event locking generate the initial lock by runing with the '--addnew' argument:\n\
            dotnet fsi EventLocker.fsx \"{assemblyFilePath}\" \"{Path.TrimEndingDirectorySeparator projectPath}\" {markerType} --addnew\n"
    
let runAddNewHashes markerType assemblyFilePath projectPath =
    let hashLockFilePath = Path.Combine(projectPath, EventLockFileName)

    let originalEventHashes =
        readEventHashesFromFile hashLockFilePath
        |> Option.defaultValue List.Empty

    let eventComparisons =
        getEventHashesForAssembly markerType hashSha assemblyFilePath projectPath
        |> compareEventHash originalEventHashes

    let mutatedEvents =
        eventComparisons
        |> List.choose
            (function
            | EventSignatureChanged (orig, changed) -> Some $"Changed Event '{orig.Type}'. Original hash: {orig.Hash} Current hash: {changed.Hash}"   
            | DeletedEventSignature (deletedEvent) -> Some $"Deleted Event {deletedEvent}"
            | _ -> None)

    let newEvents =
        eventComparisons
        |> List.choose
            (function
            | NewEventSignature (event) -> Some event
            | _ -> None)

    let numOfOriginalEventHashes = List.length originalEventHashes
    printfn $"mutatedEvents events: {mutatedEvents}" 
    printfn $"new events: {newEvents}" 

    match mutatedEvents, newEvents with
    | [], [] -> Ok $"No new events to add to the existing {numOfOriginalEventHashes} events." 
    | [], newEvents ->
        let origLookup = originalEventHashes |> eventListToMap

        let originalWithNewEvents =
            newEvents
            |> List.fold (fun acc cur -> Map.add cur.Type cur acc) origLookup
            |> Map.toList
            |> List.map snd

        saveEventHashesToFile hashLockFilePath originalWithNewEvents
        let numOfTotalEvents = List.length originalWithNewEvents

        Ok $"{numOfTotalEvents - numOfOriginalEventHashes} Event hashes added the the original {numOfOriginalEventHashes}. New total of {numOfTotalEvents}"
    | mutatedEvents, _ -> Error ($"Cannot update event hashes. Events have been mutated:\n" + String.concat "\n" mutatedEvents)

// command line running
type RunMode =
    | CompareEvents
    | ForceEventUpdates

type CommandLineOptions = { 
    AssemblyFilePath: string
    ProjectPath: string
    MarkerType: string
    RunMode: RunMode 
} 
with
    static member Create(assemblyFilePath, projectPath, markerType, runMode) = { 
        AssemblyFilePath = assemblyFilePath
        ProjectPath = projectPath
        MarkerType = markerType |> Option.defaultValue "IEvent"
        RunMode = runMode
    }

let parseArgs args =
    match args |> Array.skip 1 with
    | [| assemblyFilePath; projectPath; "--addnew"              |] -> CommandLineOptions.Create(assemblyFilePath, projectPath, None, ForceEventUpdates)
    | [| assemblyFilePath; projectPath; markerType; "--addnew"  |] -> CommandLineOptions.Create(assemblyFilePath, projectPath, (Some markerType), ForceEventUpdates)
    | [| assemblyFilePath; projectPath; markerType              |] -> CommandLineOptions.Create(assemblyFilePath, projectPath, (Some markerType), CompareEvents)
    | [| assemblyFilePath; projectPath                          |] -> CommandLineOptions.Create(assemblyFilePath, projectPath, None, CompareEvents)
    | _ -> failwith "Usage: 'EventLocker.fsx <AssemblyFilePath> <ProjectPath> [<MarkerType>] [--addnew]'\nSee readme: https://github.com/dmannock/EventLocker"

let run commandLineOptions =
    printfn "\n##########################################################################\n"
    printfn $"Running event locker with options:\n {commandLineOptions}"
    match commandLineOptions with
    | { RunMode = ForceEventUpdates } as cmd -> runAddNewHashes cmd.MarkerType cmd.AssemblyFilePath cmd.ProjectPath
    | { RunMode = CompareEvents } as cmd -> runBuildHashComparison cmd.MarkerType cmd.AssemblyFilePath cmd.ProjectPath

let printResult res =
    printfn "\n##########################################################################\n"
    match res with
    | Ok (msg) -> printfn "✔️ %s" msg
    | Error (msg) ->
        printfn "❌ %s" msg
        exit 1

// entry point
fsi.CommandLineArgs
|> parseArgs
|> run
|> printResult