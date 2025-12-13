import { gsap } from 'gsap'
import { InertiaPlugin } from 'gsap/InertiaPlugin'
import React, { useCallback, useEffect, useRef } from 'react'

import './DotGrid.css'

// eslint-disable-next-line @masknet/no-top-level
gsap.registerPlugin(InertiaPlugin)

export interface DotGridProps {
  activeColor?: string
  baseColor?: string
  className?: string
  dotSize?: number
  gap?: number
  maxSpeed?: number
  proximity?: number
  resistance?: number
  returnDuration?: number
  shockRadius?: number
  shockStrength?: number
  speedTrigger?: number
  style?: React.CSSProperties
}

interface DotCentre {
  el: InertiaDot
  x: number
  y: number
}

interface InertiaDot extends HTMLDivElement {
  _inertiaApplied: boolean
}

const DotGrid: React.FC<DotGridProps> = ({
  activeColor = '#00d8ff',
  baseColor = '#00d8ff',
  className = '',
  dotSize = 16,
  gap = 32,
  maxSpeed = 5000,
  proximity = 150,
  resistance = 750,
  returnDuration = 1.5,
  shockRadius = 250,
  shockStrength = 5,
  speedTrigger = 100,
  style,
}) => {
  const containerRef = useRef<HTMLDivElement | null>(null)
  const dotsRef = useRef<InertiaDot[]>([])
  const centresRef = useRef<DotCentre[]>([])

  const buildGrid = useCallback((): void => {
    const container = containerRef.current
    if (!container)
      return

    // eslint-disable-next-line @masknet/browser-no-set-html
    container.innerHTML = ''
    dotsRef.current = []
    centresRef.current = []

    const { clientHeight: h, clientWidth: w } = container
    const cols = Math.floor((w + gap) / (dotSize + gap))
    const rows = Math.floor((h + gap) / (dotSize + gap))
    const total = cols * rows

    for (let i = 0; i < total; i++) {
      const dot = document.createElement('div') as InertiaDot
      dot.classList.add('dot-grid__dot')
      dot._inertiaApplied = false

      gsap.set(dot, { backgroundColor: baseColor, x: 0, y: 0 })
      container.appendChild(dot)
      dotsRef.current.push(dot)
    }

    // eslint-disable-next-line @masknet/no-timer, @masknet/prefer-timer-id
    requestAnimationFrame(() => {
      centresRef.current = dotsRef.current.map((el) => {
        const r = el.getBoundingClientRect()
        return {
          el,
          x: r.left + window.scrollX + r.width / 2,
          y: r.top + window.scrollY + r.height / 2,
        }
      })
    })
  }, [dotSize, gap, baseColor])

  useEffect(() => {
    buildGrid()
    const ro = new ResizeObserver(buildGrid)
    if (containerRef.current)
      ro.observe(containerRef.current)
    return () => ro.disconnect()
  }, [buildGrid])

  useEffect(() => {
    let lastTime = 0
    let lastX = 0
    let lastY = 0

    const onMove = (e: MouseEvent): void => {
      const now = performance.now()
      const dt = now - (lastTime || now)
      const dx = e.pageX - lastX
      const dy = e.pageY - lastY
      let vx = (dx / dt) * 1000
      let vy = (dy / dt) * 1000
      let speed = Math.hypot(vx, vy)

      if (speed > maxSpeed) {
        const scale = maxSpeed / speed
        vx *= scale
        vy *= scale
        speed = maxSpeed
      }

      lastTime = now
      lastX = e.pageX
      lastY = e.pageY

      // eslint-disable-next-line @masknet/no-timer, @masknet/prefer-timer-id
      requestAnimationFrame(() => {
        // eslint-disable-next-line sonarjs/no-nested-functions
        centresRef.current.forEach(({ el, x, y }) => {
          const dist = Math.hypot(x - e.pageX, y - e.pageY)
          const interp = Math.max(0, 1 - dist / proximity)
          gsap.set(el, {
            backgroundColor: gsap.utils.interpolate(
              baseColor,
              activeColor,
              interp,
            ),
          })

          if (speed > speedTrigger && dist < proximity && !el._inertiaApplied) {
            el._inertiaApplied = true
            const pushX = x - e.pageX + vx * 0.005
            const pushY = y - e.pageY + vy * 0.005

            gsap.to(el, {
              inertia: { resistance, x: pushX, y: pushY },
              onComplete: () => {
                gsap.to(el, {
                  duration: returnDuration,
                  ease: 'elastic.out(1,0.75)',
                  x: 0,
                  y: 0,
                })
                el._inertiaApplied = false
              },
            })
          }
        })
      })
    }

    const onClick = (e: MouseEvent): void => {
      centresRef.current.forEach(({ el, x, y }) => {
        const dist = Math.hypot(x - e.pageX, y - e.pageY)
        if (dist < shockRadius && !el._inertiaApplied) {
          el._inertiaApplied = true
          const falloff = Math.max(0, 1 - dist / shockRadius)
          const pushX = (x - e.pageX) * shockStrength * falloff
          const pushY = (y - e.pageY) * shockStrength * falloff

          gsap.to(el, {
            inertia: { resistance, x: pushX, y: pushY },
            // eslint-disable-next-line sonarjs/no-nested-functions
            onComplete: () => {
              gsap.to(el, {
                duration: returnDuration,
                ease: 'elastic.out(1,0.75)',
                x: 0,
                y: 0,
              })
              el._inertiaApplied = false
            },
          })
        }
      })
    }

    window.addEventListener('mousemove', onMove)
    window.addEventListener('click', onClick)
    return () => {
      window.removeEventListener('mousemove', onMove)
      window.removeEventListener('click', onClick)
    }
  }, [
    baseColor,
    activeColor,
    proximity,
    speedTrigger,
    shockRadius,
    shockStrength,
    maxSpeed,
    resistance,
    returnDuration,
  ])

  return (
    <section
      className={`dot-grid ${className}`}
      style={
        {
          ...style,
          '--dot-gap': `${gap}px`,
          '--dot-size': `${dotSize}px`,
        } as React.CSSProperties
      }
    >
      <div className="dot-grid__wrap">
        <div className="dot-grid__container" ref={containerRef} />
      </div>
    </section>
  )
}

export default DotGrid
