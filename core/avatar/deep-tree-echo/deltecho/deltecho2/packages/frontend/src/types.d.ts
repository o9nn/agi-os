declare module '@mapbox/geojson-extent'
declare module '@deltachat/react-qr-reader'
declare module '*.module.css'
declare module '*.module.scss'
type todo = any
declare module 'react-virtualized-auto-sizer' {
import { PureComponent } from 'react'
export type Size = {
height: number
width: number
}
export type Dimensions = Size
export type AutoSizerProps = {
children: (props: Size) => React.ReactNode
className?: string
defaultHeight?: number
defaultWidth?: number
disableHeight?: boolean
disableWidth?: boolean
nonce?: string
onResize?: (info: Size) => any
style?: React.CSSProperties
[key: string]: any
}
class AutoSizer extends PureComponent<AutoSizerProps, Size> {
static defaultProps: {
onResize: () => void
disableHeight: false
disableWidth: false
style: {}
}
constructor(props: AutoSizerProps)
componentDidMount(): void
componentWillUnmount(): void
render(): JSX.Element
}
export default AutoSizer
}