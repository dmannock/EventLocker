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

let getMarkerTypeFromAssembly assemblyPath = 
    // let assemblyTypes = assemblyPath |> Assembly.LoadFile |> getAllTypes
    let assemblyTypes = getAllTypes (Assembly.GetExecutingAssembly())
    match getMarkerType "IEvent" assemblyTypes with
    | Some(t) -> t
    | None -> failwith "Unable to find marker type 'IEvent'"

let getTypesUsingMarker (markerType: Type) =
    Array.filter (fun t -> markerType.IsAssignableFrom(t) && markerType <> t)

// test loading assembly
// type IEvent = interface end
// type AnEvent = {
//     Data: string
// }
// with interface IEvent
// getMarkerTypeFromAssembly()

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

// test examples
// let hashed1 = [
//     { Type = "OrderPlaced"
//       Hash = "FDC0ECD2A178BE2FF1C8EB59236E8092E6951905765362C19DFA07F600D44A6E" }
//     { Type = "OrderDispatched"
//       Hash = "19696012208AB309DB17DAA520C6811C5332DE84495BEA041964F7BA59181B14" }
//   ]
// //same
// hashed1 |> compareEventHash hashed1

// //changed
// [
//     { Type = "OrderPlaced"
//       Hash = "CHANGED1" }
//     { Type = "OrderDispatched"
//       Hash = "19696012208AB309DB17DAA520C6811C5332DE84495BEA041964F7BA59181B14" }
// ]
// |> compareEventHash hashed1

// //new
// [
//     { Type = "OrderPlaced"
//       Hash = "FDC0ECD2A178BE2FF1C8EB59236E8092E6951905765362C19DFA07F600D44A6E" }
//     { Type = "OrderDispatched"
//       Hash = "19696012208AB309DB17DAA520C6811C5332DE84495BEA041964F7BA59181B14" }
//     { Type = "NewThingHappened"
//       Hash = "NEW1" }
// ]
// |> compareEventHash hashed1

// //deleted
// [
//     { Type = "OrderPlaced"
//       Hash = "FDC0ECD2A178BE2FF1C8EB59236E8092E6951905765362C19DFA07F600D44A6E" }
// ]
// |> compareEventHash hashed1
// |> checkEventHashesForDifferences

// // everythign should differ - 1 new, 2 deleted
// typeToEventHash id typeof<string>
// |> compareEventHash hashed1
// |> checkEventHashesForDifferences

// typeToEventHash id typeof<EventHash>

// file reading
// readEventHashesFromFile EventLockFileName 
// |> Option.map (compareEventHash hashed1)
// |> Option.map checkEventHashesForDifferences