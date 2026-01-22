import process from 'node:process'
import {
  createMoonshotaiCn,
  createLucidquery,
  createMoonshotai,
  createZaiCodingPlan,
  createAlibaba,
  createXai,
  createVultr,
  createNvidia,
  createUpstage,
  createGroq,
  createGithubCopilot,
  createMistral,
  createNebius,
  createDeepSeek,
  createAlibabaCn,
  createVenice,
  createChutes,
  createCortecs,
  createGithubModels,
  createBaseten,
  createHuggingface,
  createOpencode,
  createFastrouter,
  createGoogleGenerativeAI,
  createInception,
  createWandb,
  createOpenAI,
  createZhipuaiCodingPlan,
  createPerplexity,
  createZenmux,
  createIflowcn,
  createSynthetic,
  createDeepinfra,
  createZhipuai,
  createSubmodel,
  createZai,
  createInference,
  createRequesty,
  createMorph,
  createLmstudio,
  createFireworks,
  createModelscope,
  createLlama,
  createScaleway,
  createCerebras,
  createMinimax,
  createMinimaxi,
  createNovita,
  createSiliconFlow,
  createStepfun,
  createTencentHunyuan,
} from './create'
export const moonshotaiCn = createMoonshotaiCn(process.env.MOONSHOT_API_KEY ?? '')
export const lucidquery = createLucidquery(process.env.LUCIDQUERY_API_KEY ?? '')
export const moonshotai = createMoonshotai(process.env.MOONSHOT_API_KEY ?? '')
export const zaiCodingPlan = createZaiCodingPlan(process.env.ZHIPU_API_KEY ?? '')
export const alibaba = createAlibaba(process.env.DASHSCOPE_API_KEY ?? '')
export const xai = createXai(process.env.XAI_API_KEY ?? '')
export const vultr = createVultr(process.env.VULTR_API_KEY ?? '')
export const nvidia = createNvidia(process.env.NVIDIA_API_KEY ?? '')
export const upstage = createUpstage(process.env.UPSTAGE_API_KEY ?? '')
export const groq = createGroq(process.env.GROQ_API_KEY ?? '')
export const githubCopilot = createGithubCopilot(process.env.GITHUB_TOKEN ?? '')
export const mistral = createMistral(process.env.MISTRAL_API_KEY ?? '')
export const nebius = createNebius(process.env.NEBIUS_API_KEY ?? '')
export const deepseek = createDeepSeek(process.env.DEEPSEEK_API_KEY ?? '')
export const alibabaCn = createAlibabaCn(process.env.DASHSCOPE_API_KEY ?? '')
export const venice = createVenice(process.env.VENICE_API_KEY ?? '')
export const chutes = createChutes(process.env.CHUTES_API_KEY ?? '')
export const cortecs = createCortecs(process.env.CORTECS_API_KEY ?? '')
export const githubModels = createGithubModels(process.env.GITHUB_TOKEN ?? '')
export const baseten = createBaseten(process.env.BASETEN_API_KEY ?? '')
export const huggingface = createHuggingface(process.env.HF_TOKEN ?? '')
export const opencode = createOpencode(process.env.OPENCODE_API_KEY ?? '')
export const fastrouter = createFastrouter(process.env.FASTROUTER_API_KEY ?? '')
export const google = createGoogleGenerativeAI(process.env.GOOGLE_GENERATIVE_AI_API_KEY ?? process.env.GEMINI_API_KEY ?? '')
export const inception = createInception(process.env.INCEPTION_API_KEY ?? '')
export const wandb = createWandb(process.env.WANDB_API_KEY ?? '')
export const openai = createOpenAI(process.env.OPENAI_API_KEY ?? '')
export const zhipuaiCodingPlan = createZhipuaiCodingPlan(process.env.ZHIPU_API_KEY ?? '')
export const perplexity = createPerplexity(process.env.PERPLEXITY_API_KEY ?? '')
export const zenmux = createZenmux(process.env.ZENMUX_API_KEY ?? '')
export const iflowcn = createIflowcn(process.env.IFLOW_API_KEY ?? '')
export const synthetic = createSynthetic(process.env.SYNTHETIC_API_KEY ?? '')
export const deepinfra = createDeepinfra(process.env.DEEPINFRA_API_KEY ?? '')
export const zhipuai = createZhipuai(process.env.ZHIPU_API_KEY ?? '')
export const submodel = createSubmodel(process.env.SUBMODEL_INSTAGEN_ACCESS_KEY ?? '')
export const zai = createZai(process.env.ZHIPU_API_KEY ?? '')
export const inference = createInference(process.env.INFERENCE_API_KEY ?? '')
export const requesty = createRequesty(process.env.REQUESTY_API_KEY ?? '')
export const morph = createMorph(process.env.MORPH_API_KEY ?? '')
export const lmstudio = createLmstudio(process.env.LMSTUDIO_API_KEY ?? '')
export const fireworks = createFireworks(process.env.FIREWORKS_API_KEY ?? '')
export const modelscope = createModelscope(process.env.MODELSCOPE_API_KEY ?? '')
export const llama = createLlama(process.env.LLAMA_API_KEY ?? '')
export const scaleway = createScaleway(process.env.SCALEWAY_API_KEY ?? '')
export const cerebras = createCerebras(process.env.CEREBRAS_API_KEY ?? '')
export const minimax = createMinimax(process.env.MINIMAX_API_KEY ?? '')
export const minimaxi = createMinimaxi(process.env.MINIMAX_API_KEY ?? '')
export const novita = createNovita(process.env.NOVITA_API_KEY ?? '')
export const siliconFlow = createSiliconFlow(process.env.SILICON_FLOW_API_KEY ?? '')
export const stepfun = createStepfun(process.env.STEPFUN_API_KEY ?? '')
export const tencentHunyuan = createTencentHunyuan(process.env.TENCENT_HUNYUAN_API_KEY ?? '')