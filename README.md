# Event Locker
We have immutable types at runtime, why not at compile time?

This script ensures types have not been mutated. This is useful with Events when EventSourcing and was it's original use case. You may find other usages since it works with any type.

## Summary
- Copy 1 file, 1 snippet, run, displays comparison results
- Runs on netcore, 0 package dependencies
- Compatible with C#, F# types

## Requirements
Runs on any dotnet environment that supports fsi ([dotnet core SDK](https://dotnet.microsoft.com/download) out of the box).

## Usage
### 1. Check your code
Ensure your event types implement a common marker type e.g. ```IEvent``` in the examples below.

#### Classes (C# / F#)
```c#
//marker interface
public interface IEvent 
{
}

//example of 1 Event using the marker interface
public class OrderPlacedEvent : IEvent
{
}
```
#### Discriminated Unions (F#)
```c#
//marker interface
type IEvent = interface end

//example of 2 Events using the marker interface
type Events =
  | OrderPlacedEvent of OrderPlacedEvent
  | InterestingThingHappenedEvent of InterestingThingHappenedEvent
with interface IEvent
```

### 2. Add the script to your repository

### 3. Configure the script to run after each build
Copy the below snipped in to the project files that contain events replacing the ```PathToEventLockerScriptHere``` with the location from step 2.
```xml
<Target Name="PostBuild" AfterTargets="PostBuildEvent">
  <Exec Command="dotnet fsi <PathToEventLockerScriptHere> $(TargetPath) $(ProjectDir)" />
</Target>
```
Alternatively add to you build script / process of choice.

Building the project / solution should now fail the build with:
```
No event lock file found. To start using event locking generate the initial lock by runing with the '--addnew' argument:
dotnet fsi EventLocker.fsx <AssemblyFilePath> <ProjectPath> --addnew
```
### 4. Generate the initial lock file

Use the last line provided to call the script (with the ```--addnew``` argument)
```cmd
EventLocker.fsx <AssemblyFilePath> <ProjectPath> [<MarkerType>] [--addnew]
```
- AssemblyFilePath - the dotnet dll that you want to lock events on
- ProjectPath - the path where the has lock file will be sroted for this assembly
- MarkerType - optional marker type, defaults to ```IEvent```

### 5. Adding events

Build as normal and when a new event is added run with the ```--addnew``` argument to track the new events. 
Mutating an existing event will still fail the build.

## Running Success / Failure Cases
The script will indicate an error (in order to fail a build) if:
1. A hask lock file has not yet been generated for the assembly
2. Events have been mutated (deleted or modified)
3. A new event has been added but the event hasn't been registered via the --addnew argument

## Todos / ideas / roadmap
- Feedback from usage of the initial POC version
- Integrate with editors / faster feedback (again from above feedback)
- Further features would benefit from unit tests over the REPL
- Examples

## Maintainers
[@dmannock](https://twitter.com/dmannock)