open System
type EventHash =  {
    Type: string 
    Hash: string 
}

//event hash lock file comparison
type EventComparison =
    | SameEventSignature of EventHash
    | NewEventSignature of EventHash
    | DeletedEventSignature of EventHash
    | EventSignatureChanged of Original: EventHash * Current: EventHash

let eventListToMap list = list |> List.map (fun x -> x.Type, x) |> Map.ofList

type Compare = EventHash list -> EventHash list -> EventComparison list
let compareEventHash :Compare =
    fun originalHashLock currentHashes ->
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
