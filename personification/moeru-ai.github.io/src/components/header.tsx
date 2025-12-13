import { Icon } from '@iconify/react'
import { Box, IconButton, Text, Tooltip } from '@radix-ui/themes'

export const Header = () => {
  const navigate = (url: string) => () => window.open(url, '_blank', 'noopener')

  return (
    <Box className="absolute w-dvw z-10">
      <Box className="w-full flex p-2 lg:p-4 gap-2 justify-center items-center">
        <Text className="font-doto leading-none" size="6">MOERU AI</Text>
        <Box className="grow" />
        <Tooltip content="Blog">
          <IconButton className="hover:cursor-pointer" data-test-id="blog" onClick={navigate('https://blog.moeru.ai')} variant="soft">
            <Icon icon="simple-icons:rss" />
          </IconButton>
        </Tooltip>
        <Tooltip content="NPM">
          <IconButton className="hover:cursor-pointer" data-test-id="npm" onClick={navigate('https://npmjs.com/org/moeru-ai')} variant="soft">
            <Icon icon="simple-icons:npm" />
          </IconButton>
        </Tooltip>
        <Tooltip content="Hugging Face">
          <IconButton className="hover:cursor-pointer" data-test-id="huggingface" onClick={navigate('https://huggingface.com/moeru-ai')} variant="soft">
            <Icon icon="simple-icons:huggingface" />
          </IconButton>
        </Tooltip>
        <Tooltip content="GitHub">
          <IconButton className="hover:cursor-pointer mr-0" data-test-id="github" onClick={navigate('https://github.com/moeru-ai')} variant="soft">
            <Icon icon="simple-icons:github" />
          </IconButton>
        </Tooltip>
      </Box>
    </Box>
  )
}
