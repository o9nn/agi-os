export class NoBinaryFoundError extends Error {
public constructor(message: string = "NoBinaryFoundError") {
super(message);
}
}