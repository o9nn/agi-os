import { useLocalStorage } from '@vueuse/core'
import { defineStore } from 'pinia'
import { computed, nextTick, ref } from 'vue'
import { useProvidersStore } from './providers'
export const useOnboardingStore = defineStore('onboarding', () => {
  const providersStore = useProvidersStore()
  const hasCompletedSetup = useLocalStorage('onboarding/completed', false)
  const hasSkippedSetup = useLocalStorage('onboarding/skipped', false)
  const shouldShowSetup = ref(false)
  const hasEssentialProviderConfigured = computed(() => {
    const essentialProviders = ['openai', 'anthropic', 'google-generative-ai', 'openrouter-ai', 'ollama', 'deepseek', 'openai-compatible']
    return essentialProviders.some(providerId => providersStore.configuredProviders[providerId])
  })
  const needsOnboarding = computed(() => {
    if (hasCompletedSetup.value || hasSkippedSetup.value) {
      console.warn('Onboarding already completed or skipped')
      return false
    }
    if (hasEssentialProviderConfigured.value) {
      console.warn('Essential provider already configured, no onboarding needed')
      return false
    }
    return true
  })
  async function initializeSetupCheck() {
    if (needsOnboarding.value) {
      await nextTick()
      shouldShowSetup.value = true
    }
  }
  function markSetupCompleted() {
    hasCompletedSetup.value = true
    hasSkippedSetup.value = false
    shouldShowSetup.value = false
  }
  function markSetupSkipped() {
    hasSkippedSetup.value = true
    shouldShowSetup.value = false
  }
  function resetSetupState() {
    hasCompletedSetup.value = false
    hasSkippedSetup.value = false
    shouldShowSetup.value = false
  }
  function forceShowSetup() {
    shouldShowSetup.value = true
  }
  return {
    hasCompletedSetup,
    hasSkippedSetup,
    shouldShowSetup,
    hasEssentialProviderConfigured,
    needsOnboarding,
    initializeSetupCheck,
    markSetupCompleted,
    markSetupSkipped,
    resetSetupState,
    forceShowSetup,
  }
})