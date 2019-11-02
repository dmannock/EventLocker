open System

open Microsoft.FSharp.Reflection
open System.Reflection
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
        //get public signature for type
        |> getTypesPublicSignature
        //signature to string
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