import {fileURLToPath} from "url";
import path from "path";
import chalk from "chalk";
import {getLlama, LlamaChatSession, resolveModelFile} from "node-llama-cpp";
const __dirname = path.dirname(fileURLToPath(import.meta.url));
const modelsDirectory = path.join(__dirname, "..", "models");
const llama = await getLlama();
console.log(chalk.yellow("Resolving model file..."));
const modelPath = await resolveModelFile(
    "{{modelUriOrFilename|escape}}",
    modelsDirectory
);
console.log(chalk.yellow("Loading model..."));
const model = await llama.loadModel({modelPath});
console.log(chalk.yellow("Creating context..."));
const context = await model.createContext({
    contextSize: {max: 8096} 
});
const session = new LlamaChatSession({
    contextSequence: context.getSequence()
});
console.log();
const q1 = "Hi there, how are you?";
console.log(chalk.yellow("User: ") + q1);
process.stdout.write(chalk.yellow("AI: "));
const a1 = await session.prompt(q1, {
    onResponseChunk(chunk) {
        if (chunk.type === "segment" && chunk.segmentStartTime != null)
            process.stdout.write(chalk.bold(` [segment start: ${chunk.segmentType}] `));
        process.stdout.write(chunk.text);
        if (chunk.type === "segment" && chunk.segmentEndTime != null)
            process.stdout.write(chalk.bold(` [segment end: ${chunk.segmentType}] `));
    }
});
process.stdout.write("\n");
console.log(chalk.yellow("Consolidated AI answer: ") + a1);
console.log();
const q2 = "Summarize what you said";
console.log(chalk.yellow("User: ") + q2);
const a2 = await session.prompt(q2);
console.log(chalk.yellow("AI: ") + a2);
console.log();
const q3 = "What are the verbs in this sentence: 'The cat sat on the mat'";
console.log(chalk.yellow("User: ") + q3);
const responseGrammar = await llama.createGrammarForJsonSchema({
    type: "object",
    properties: {
        verbs: {
            type: "array",
            items: {
                type: "string"
            }
        }
    }
});
const a3 = await session.prompt(q3, {grammar: responseGrammar});
const parsedResponse = responseGrammar.parse(a3);
console.log(chalk.yellow("AI:"), parsedResponse.verbs);
console.log();
if (parsedResponse.verbs.length > 0) {
    const q4 = `Define the verb "${parsedResponse.verbs[0]}"`;
    console.log(chalk.yellow("User: ") + q4);
    const a4 = await session.prompt(q4);
    console.log(chalk.yellow("AI: ") + a4);
    console.log();
} else {
    const q4 = "Are you sure there are no verbs in the sentence?";
    console.log(chalk.yellow("User: ") + q4);
    const a4 = await session.prompt(q4);
    console.log(chalk.yellow("AI: ") + a4);
    console.log();
}