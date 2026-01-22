import { createFetch } from '@xsmcp/server-http'
import { serve } from 'srvx'
import { server } from './server'
serve({
  fetch: createFetch(server),
  port: 3001,
})