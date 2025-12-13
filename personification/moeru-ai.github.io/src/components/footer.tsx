import { Box, Text } from '@radix-ui/themes'
import { useMemo } from 'react'

import { Members } from './members'

export const Footer = () => {
  // eslint-disable-next-line @masknet/no-unsafe-date
  const year = useMemo(() => new Date().getFullYear(), [])

  return (
    <Box className="absolute bottom-0 w-dvw z-10">
      <Box className="w-full flex p-2 lg:p-4 gap-2 justify-center items-center">
        <Members />
        <Box className="grow" />
        <Text className="font-pixel text-end leading-none" color="gray">
          &#x00A9;
          {' '}
          Moeru AI
          {' '}
          {year}
        </Text>
      </Box>
    </Box>
  )
}
