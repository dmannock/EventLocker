# Event Locker (POC)
### Idea
Script to run on any dotnet environment that will check types (i.e. Events when EventSourcing) and ensure they have not been mutated since they were added (marked immutable).

Currently the events are marked by inheriting from a marker type called 'IEvent'

Compatible with: C#, F#

## Running
The script will indicate an error if (in order to fail a vuild, etc)
1. A hask lock file has not yet been generated for the assembly
2. Events have been mutated (deleted or modified)
3. A new event has been added but the event hasn't been registered via the --addnew argument

```cmd
EventLocker.fsx <assemblyPath> <hashLockFilePath> [--addnew]
```
- AssemblyPath - the dotnet dll that you want to lock events on
- HashLockFilePath - the path where the has lock file will be sroted for this assembly
### Optional
- --addnew - Generates initial event locks or add new events to an existing has lock.

## Usage
An example of failing the build by adding the below snipped to a project file.
```
<Target Name="PostBuild" AfterTargets="PostBuildEvent">
  <Exec Command="dotnet fsi EventLocker.fsx $(TargetPath) $(ProjectDir)" />
</Target>
```
