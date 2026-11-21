import { dotnet } from './_framework/dotnet.js'

const runtime = await dotnet.withApplicationArguments().create()
await runtime.runMain()
