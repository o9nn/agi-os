import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'

import { App } from './app.tsx'

// eslint-disable-next-line @masknet/no-top-level
createRoot(document.getElementById('root') as HTMLCanvasElement).render(
  <StrictMode>
    <App />
  </StrictMode>,
)
