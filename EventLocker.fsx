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

let getTypesUsingMarker (markerType: Type) =
    Array.filter (fun t -> markerType.IsAssignableFrom(t) && markerType <> t)

open Microsoft.FSharp.Reflection
// Taken from https://github.com/dmannock/FSharpUnionHelpers
let getUnionCaseRecord (uc: UnionCaseInfo) = uc.GetFields() |> Array.tryHead |> Option.map (fun i -> i.PropertyType)
// curently only cares about classes / records / unions top-level public signature
let rec getTypesPublicSignature (t: Type) = 
    let bindingFlags = BindingFlags.Public ||| BindingFlags.Instance
    let getRecordFields t = FSharpType.GetRecordFields t |> Seq.map (fun x -> x.Name, x.PropertyType.Name)
    if t.IsPrimitive then Seq.singleton (t.Name, t.Name)
    else if t = typeof<String> then Seq.singleton (t.Name, t.Name)
    else if FSharpType.IsRecord t then getRecordFields t
    else if FSharpType.IsUnion t then 
        FSharpType.GetUnionCases(t)
        |> Seq.choose (getUnionCaseRecord >> Option.map getTypesPublicSignature)
        |> Seq.collect id               
    else 
        seq { 
            yield! t.GetProperties(bindingFlags) |> Seq.map (fun x -> x.Name, x.PropertyType.Name)
            yield! t.GetFields(bindingFlags) |> Seq.map (fun x -> x.Name, x.FieldType.Name)
        }
    |> Seq.sortBy fst

let toSignatureString typeName signature =
    let fieldString = signature |> Seq.map (fun (name, t) -> sprintf "%s:%s" name t)
    typeName + "=" + String.Join("#", fieldString) 

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
        |> toSignatureString theType.Name
        |> hashFn
    createEventHash t.Name (hashType  t)
    |> List.singleton 

let getEventHashesForAssembly hashFn assemblyPath =
    let assemblyTypes = loadAssemblyTypes assemblyPath
    let markerType = getMarkerTypeFromAssembly assemblyTypes
    let typesForMarker = getTypesUsingMarker markerType assemblyTypes
    typesForMarker
    |> List.ofArray
    |> List.collect (typeToEventHash hashFn)

//event hash lock file comparison
type EventComparison =
    | SameEventSignature of EventHash
    | NewEventSignature of EventHash
    | DeletedEventSignature of EventHash
    | EventSignatureChanged of Original: EventHash * Current: EventHash

let eventListToMap list = list |> List.map (fun x -> x.Type, x) |> Map.ofList

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

let checkEventHashesForDifferences eventComparisons =
    eventComparisons
    |> List.choose (
        function 
        | SameEventSignature(_) -> None
        | NewEventSignature(newEvent) -> Some (sprintf "New Event: %A" newEvent)
        | DeletedEventSignature(deletedEvent) -> Some (sprintf "Deleted Event: %A" deletedEvent)
        | EventSignatureChanged(orig, changed) -> Some (sprintf "Changed Event '%s'. Original hash: %s Current hash: %s" orig.Type orig.Hash changed.Hash)
    )
    |> function
    | [] -> Ok ()
    | errors -> Error errors

let addNewHashes hashLockFilePath originalEventHashes eventComparisons =
    let mutatedEvents = eventComparisons |> List.choose (function 
            | EventSignatureChanged(orig, changed) -> Some()
            | DeletedEventSignature(deletedEvent) -> Some()
            | _ -> None)
    let newEvents = eventComparisons |> List.choose (function 
            | NewEventSignature(event) -> Some event
            | _ -> None)
    match mutatedEvents, newEvents with
    | [], [] -> Ok <| sprintf "No new events to add."
    | [], newEvents ->
        let origLookup = originalEventHashes |> eventListToMap
        let originalWithNewEvents = 
            newEvents 
            |> List.fold (fun acc cur -> Map.add cur.Type cur acc) origLookup
            |> Map.toList
            |> List.map snd
        saveEventHashesToFile hashLockFilePath originalWithNewEvents
        Ok <| sprintf "%i Event hashes added the the original %i. New total of %i" (List.length newEvents) (List.length originalEventHashes) (List.length originalWithNewEvents)
    | mutatedEvents, _ -> Error <| sprintf "Cannot update event hashes. Events have been mutated:\n"

// test examples
// file reading & comparison
// let origEvents = 
//     readEventHashesFromFile EventLockFileName 
//     |> function
//         | Some(hashes) -> hashes
//         | None -> failwithf "No event lock file found at %s\n" EventLockFileName
// let currentEvents = getEventHashesForAssembly hashSha EventLockFileName
// let compared = compareEventHash origEvents currentEvents
//compared |> checkEventHashesForDifferences

// add new events to file if valid to do so
// compared |> addNewHashes EventLockFileName origEvents
