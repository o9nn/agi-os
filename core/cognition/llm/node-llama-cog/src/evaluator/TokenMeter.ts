export class TokenMeter {
    private _inputTokens: number = 0;
    private _outputTokens: number = 0;
    public get usedInputTokens() {
        return this._inputTokens;
    }
    public get usedOutputTokens() {
        return this._outputTokens;
    }
    public getState(): TokenMeterState {
        return {
            usedInputTokens: this.usedInputTokens,
            usedOutputTokens: this.usedOutputTokens
        };
    }
    public useTokens(tokens: number, type: "input" | "output") {
        if (tokens < 0)
            throw new RangeError("Tokens cannot be negative");
        else if (tokens === 0)
            return;
        if (type === "input")
            this._inputTokens += tokens;
        else if (type === "output")
            this._outputTokens += tokens;
        else {
            void (type satisfies never);
            throw new TypeError(`Unknown token type: ${type}`);
        }
    }
    public diff(meter: TokenMeter | TokenMeterState) {
        return TokenMeter.diff(this, meter);
    }
    public static useTokens(
        meters: null | undefined | TokenMeter | readonly TokenMeter[] | ReadonlySet<TokenMeter>,
        tokens: number,
        type: "input" | "output"
    ) {
        if (meters == null)
            return;
        if (meters instanceof TokenMeter)
            meters.useTokens(tokens, type);
        else {
            for (const meter of meters)
                meter.useTokens(tokens, type);
        }
    }
    public static diff(
        meter1: TokenMeter | TokenMeterState,
        meter2: TokenMeter | TokenMeterState
    ) {
        return {
            usedInputTokens: meter1.usedInputTokens - meter2.usedInputTokens,
            usedOutputTokens: meter1.usedOutputTokens - meter2.usedOutputTokens
        };
    }
}
export type TokenMeterState = {
    usedInputTokens: number,
    usedOutputTokens: number
};