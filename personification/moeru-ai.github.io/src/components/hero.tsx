// import { Box } from '@radix-ui/themes'

import { gray, mint } from '@radix-ui/colors'

import DotGrid from './reactbits/DotGrid/DotGrid'
import Lanyard from './reactbits/Lanyard/Lanyard'
import TextPressure from './reactbits/TextPressure/TextPressure'

export const Hero = () => (
  <div className="h-dvh w-dvw children:absolute children:h-dvh children:w-dvw" style={{ backgroundColor: 'var(--gray-a2)' }}>
    <DotGrid
      activeColor={mint.mint9}
      baseColor={gray.gray3}
      dotSize={5}
      gap={50}
    />
    <div className="flex flex-col justify-center! items-center pt-32 lg:pt-28 my-auto">
      <TextPressure
        italic={true}
        minFontSize={96}
        text="does kindness plus"
        textColor={gray.gray12}
      />
      <TextPressure
        italic={true}
        minFontSize={96}
        text="sadness equal to zero?"
        textColor={gray.gray12}
      />
    </div>
    <Lanyard gravity={[0, -40, 0]} position={[0, 0, 20]} />
  </div>
)
