import { getLogger } from '../../../../shared/logger'
const log = getLogger('renderer/VisionCapabilities')
export class VisionCapabilities {
private static instance: VisionCapabilities
private static DISABLED = false
private tensorflowLoaded: boolean = false
private modelLoaded: boolean = false
private model: any = null
private labels: string[] = []
private initializationPromise: Promise<void> | null = null
private constructor() {
}
public static getInstance(): VisionCapabilities {
if (!VisionCapabilities.instance) {
VisionCapabilities.instance = new VisionCapabilities()
}
return VisionCapabilities.instance
}
public static disable(): void {
VisionCapabilities.DISABLED = true
log.info('Vision capabilities disabled - TensorFlow.js will not be loaded')
}
private async initialize(): Promise<void> {
if (this.initializationPromise) {
return this.initializationPromise
}
this.initializationPromise = this.doInitialize()
return this.initializationPromise
}
private async doInitialize(): Promise<void> {
try {
log.info('Starting lazy load of TensorFlow.js...')
const tf = await import('@tensorflow/tfjs')
this.tensorflowLoaded = true
log.info('TensorFlow.js loaded successfully')
const mobilenet = await import('@tensorflow-models/mobilenet')
this.model = await mobilenet.load()
this.modelLoaded = true
log.info('MobileNet model loaded successfully')
} catch (error) {
log.error('Failed to initialize vision capabilities:', error)
this.initializationPromise = null
}
}
public async analyzeImage(
imageUrl: string
): Promise<{ className: string; probability: number }[]> {
if (!this.tensorflowLoaded || !this.modelLoaded || !this.model) {
log.warn('Vision model not loaded yet, trying to initialize')
await this.initialize()
if (!this.modelLoaded) {
throw new Error('Vision capabilities are not available')
}
}
try {
const img = new Image()
img.crossOrigin = 'anonymous'
img.src = imageUrl
await new Promise(resolve => {
img.onload = resolve
})
const predictions = await this.model.classify(img)
return predictions
} catch (error) {
log.error('Error analyzing image:', error)
throw new Error('Failed to analyze image')
}
}
public async generateImageDescription(imageUrl: string): Promise<string> {
try {
const predictions = await this.analyzeImage(imageUrl)
if (predictions.length === 0) {
return "I couldn't identify anything specific in this image."
}
const topPrediction = predictions[0]
let description = `I can see that this image shows ${topPrediction.className}`
if (predictions.length > 1) {
description += `. It might also contain ${predictions[1].className}`
if (predictions.length > 2) {
description += ` and possibly ${predictions[2].className}`
}
}
return description
} catch (error) {
log.error('Error generating image description:', error)
return "I'm unable to analyze this image at the moment."
}
}
public isAvailable(): boolean {
return this.tensorflowLoaded && this.modelLoaded
}
}