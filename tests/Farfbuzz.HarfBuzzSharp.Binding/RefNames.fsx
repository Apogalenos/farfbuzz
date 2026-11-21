open System
open System.Reflection

for path in fsi.CommandLineArgs |> Array.skip 1 do
    for name in Assembly.LoadFile(IO.Path.GetFullPath path).GetReferencedAssemblies() do
        Console.WriteLine(name.FullName)
