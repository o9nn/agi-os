import {defineConfig, configDefaults} from "vitest/config";
export default defineConfig({
    test: {
        exclude: [
            ...configDefaults.exclude,
            "./llama"
        ],
        pool: "forks",
        maxWorkers: 1,
        minWorkers: 1,
        maxConcurrency: 1,
        poolOptions: {
            forks: {
                minForks: 1,
                maxForks: 1,
                singleFork: true
            }
        },
        snapshotSerializers: [
            "./test/utils/helpers/llamaTextSerializer.ts",
            "./test/utils/helpers/SpecialTokensTextSerializer.ts",
            "./test/utils/helpers/SpecialTokenSerializer.ts"
        ],
        setupFiles: ["./test/utils/helpers/testSetup.ts"]
    }
});