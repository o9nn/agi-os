import {GbnfJsonDefList, GbnfJsonSchema, GbnfJsonSchemaToType} from "../../../utils/gbnfJson/types.js";
import {ChatSessionModelFunction} from "../../../types.js";
export function defineChatSessionFunction<
    const Params extends GbnfJsonSchema<Defs>,
    const Defs extends GbnfJsonDefList<Defs>
>({
    description,
    params,
    handler
}: {
    description?: string,
    params?: Readonly<Params> & GbnfJsonSchema<Defs>,
    handler: (params: GbnfJsonSchemaToType<NoInfer<Params>>) => Promise<any> | any
}): ChatSessionModelFunction<NoInfer<Params>> {
    return {
        description,
        params,
        handler
    };
}