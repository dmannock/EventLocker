# Event Locker
Script that ensures types have not been mutated (i.e. Events when EventSourcing) since they were added (marked immutable).

Currently the events are marked by inheriting from a marker type called 'IEvent'

Runs on any dotnet environment that supports fsi.
Compatible with: C#, F#

## Usage
### 1. Check your code
Ensure your event types implement the marker interface ```IEvent``` like the examples below.

#### Classes (C# / F#)
```c#
public class OrderPlacedEvent : IEvent
{
}
```
#### Discriminated Unions (F#)
```c#
//marker interface
type IEvent = interface end
type Events =
  | OrderPlacedEvent of OrderPlacedEvent
  | InterestingThingHappenedEvent of InterestingThingHappenedEvent
with interface IEvent
```

### 2. Add the script to your repository

### 3. Configure the script to run after each build
Copy the below snipped in to the project files that contains your events replacing the ```PathToEventLockerScriptHere``` with the location from step 2.
```xml
<Target Name="PostBuild" AfterTargets="PostBuildEvent">
  <Exec Command="dotnet fsi <PathToEventLockerScriptHere> $(TargetPath) $(ProjectDir)" />
</Target>
```
Alternatively add to you build script / process of choice.

Building the project / solution should now fail the build with:
```
No event lock file found. To start using event locking generate the initial lock by runing with the '--addnew' argument:
dotnet fsi EventLocker.fsx <AssemblyPath> <HashLockFilePath> --addnew
```
### 4. Generate the initial lock file

Use the last line provided to call the script (with the ```--addnew``` argument)
```cmd
EventLocker.fsx <AssemblyPath> <HashLockFilePath> [--addnew]
```
- AssemblyPath - the dotnet dll that you want to lock events on
- HashLockFilePath - the path where the has lock file will be sroted for this assembly

### 5. Adding events

Build as normal and when a new event is added run with the```--addnew``` argument to track the new events. 
Mutating existing event will still fail the build.

## Running Success / Failure Cases
The script will indicate an error (in order to fail a build) if:
1. A hask lock file has not yet been generated for the assembly
2. Events have been mutated (deleted or modified)
3. A new event has been added but the event hasn't been registered via the --addnew argument

## Todos
- Handle any other types that may not be supported: nesting, etc
- Ensure the public type signature to strings conversion doesn't have collisions
- Union case grouping pushed up earlier / to signature type generation
- feedback from usage of the initial POC version