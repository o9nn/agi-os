import path from "node:path";
import {fileURLToPath} from "node:url";
import {defineConfig} from "vite";
import electron from "vite-plugin-electron/simple";
import react from "@vitejs/plugin-react";
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const electronExternalModules = ["node-llama-cpp", "lifecycle-utils"];
export default defineConfig({
    esbuild: {
        target: "es2022"
    },
    optimizeDeps: {
        exclude: electronExternalModules,
        esbuildOptions: {
            target: "es2022"
        }
    },
    build: {
        outDir: path.join(__dirname, "dist"),
        target: "es2022"
    },
    root: path.join(__dirname, "src"),
    publicDir: path.join(__dirname, "public"),
    plugins: [
        react(),
        electron({
            main: {
                entry: path.join(__dirname, "electron/index.ts"),
                onstart({startup}) {
                    if (process.env["ENABLE_INSPECT"] === "true")
                        return startup([".", "--inspect"]);
                    return startup(["."]);
                },
                vite: {
                    build: {
                        target: "es2022",
                        outDir: path.join(__dirname, "dist-electron"),
                        rollupOptions: {
                            external: electronExternalModules
                        }
                    }
                }
            },
            preload: {
                input: path.join(__dirname, "electron/preload.ts"),
                vite: {
                    build: {
                        target: "es2022",
                        outDir: path.join(__dirname, "dist-electron")
                    }
                }
            },
            renderer: process.env.NODE_ENV === "test"
                ? undefined
                : {}
        })
    ]
});