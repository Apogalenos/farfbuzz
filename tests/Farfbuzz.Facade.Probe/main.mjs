import { dotnet } from './_framework/dotnet.js';

const exitCode = await dotnet
    .withApplicationArguments(...process.argv.slice(2))
    .run();

process.exit(exitCode);
