import type { SFCDescriptor } from '@vue/compiler-sfc'
export function isUseInlineTemplate(
descriptor: SFCDescriptor,
): boolean {
return (
!!descriptor.scriptSetup
&& !descriptor.template?.src
)
}