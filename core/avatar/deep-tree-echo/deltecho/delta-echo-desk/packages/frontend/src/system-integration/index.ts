import { initBadgeCounter } from './badge-counter'
import { initNotifications } from './notifications'
import { initWebxdc } from './webxdc'
export default function initSystemIntegration() {
initNotifications()
initBadgeCounter()
initWebxdc()
}