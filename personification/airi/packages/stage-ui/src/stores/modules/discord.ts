import { useLocalStorage } from '@vueuse/core'
import { defineStore } from 'pinia'
import { computed } from 'vue'
import { useConfiguratorByModsChannelServer } from '../configurator'
export const useDiscordStore = defineStore('discord', () => {
  const configurator = useConfiguratorByModsChannelServer()
  const enabled = useLocalStorage('settings/discord/enabled', false)
  const token = useLocalStorage('settings/discord/token', '')
  function saveSettings() {
    configurator.updateFor('discord', {
      token: token.value,
      enabled: enabled.value,
    })
  }
  const configured = computed(() => {
    return !!token.value.trim()
  })
  return {
    enabled,
    token,
    configured,
    saveSettings,
  }
})