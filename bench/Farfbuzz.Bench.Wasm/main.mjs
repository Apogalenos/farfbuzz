import { writeFileSync, mkdirSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { dotnet } from './_framework/dotnet.js';

const arguments_ = process.argv.slice(2);
const jsonIndex = arguments_.indexOf('--json');
const jsonPath = jsonIndex >= 0 ? resolve(arguments_[jsonIndex + 1]) : undefined;
const jsonPrefix = 'BENCH-JSON ';

const exitCode = await dotnet
    .withApplicationArguments(...arguments_)
    .withModuleConfig({
        print: line => {
            if (line.startsWith(jsonPrefix) && jsonPath !== undefined) {
                mkdirSync(dirname(jsonPath), { recursive: true });
                writeFileSync(jsonPath, line.slice(jsonPrefix.length));
            } else {
                console.log(line);
            }
        }
    })
    .run();

process.exit(exitCode);
