open System
type EventHash =  {
    Type: string 
    Hash: string 
}
let createEventHash t h = {Type = t; Hash = h}

// placeholder fn signature for primitive types currently
let typeToEventHash (hashFn: string -> string) (t: Type) =
    //get public signature for type
    let getPublicSignatureForType (theType: Type) = (theType.Name, theType.Name)
    //signature to string
    let toSignatureString (name,t) = sprintf "%s:%s" name t
    //hash signature string
    let hashType theType =
        theType
        |> getPublicSignatureForType
        |> toSignatureString
        |> hashFn
    createEventHash t.Name (hashType  t)
    |> Array.singleton 

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

// // //deleted
// [
//     { Type = "OrderPlaced"
//       Hash = "FDC0ECD2A178BE2FF1C8EB59236E8092E6951905765362C19DFA07F600D44A6E" }
// ]
// |> compareEventHash hashed1
// |> checkEventHashesForDifferences
