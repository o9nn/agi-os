import { Streamdown } from "streamdown";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Layers, Zap, GitBranch } from "lucide-react";

const content = `
# J-Surface Elementary Differential Reactor

The **J-Surface Elementary Differential Reactor** is the core dynamical engine of the Deep Tree Echo system. It unites continuous gradient flow with discrete B-series integration on a symplectic manifold.

## Mathematical Foundation

The reactor evolves a state vector $\\psi$ according to the unified equation:

$$
\\frac{\\partial \\psi}{\\partial t} = \\alpha \\cdot J(\\psi) \\cdot \\nabla H(\\psi) + \\beta \\cdot \\sum_{\\tau \\in T} \\frac{b(\\tau)}{\\sigma(\\tau)} \\cdot F(\\tau)(\\psi)
$$

### Components

1. **Symplectic Structure** $J(\\psi)$:
   - Defines the geometry of the phase space
   - Preserves energy and volume ($J^T J = -I$)
   - Ensures reversible dynamics

2. **Hamiltonian Gradient** $\\nabla H(\\psi)$:
   - Represents the energy landscape
   - Drives the system towards lower energy states
   - Encodes the optimization objective

3. **B-Series Integration**:
   - $\\tau$: A rooted tree from the set $T$
   - $b(\\tau)$: B-series coefficient for tree $\\tau$
   - $\\sigma(\\tau)$: Symmetry factor of tree $\\tau$
   - $F(\\tau)(\\psi)$: Elementary differential operator

## Implementation Details

The reactor is implemented in \`AdvancedJSurfaceElementaryDifferentials.jl\`. It maintains a symplectic structure matrix and computes elementary differentials recursively.

### Elementary Differentials

For a rooted tree $\\tau = [\\tau_1, \\dots, \\tau_k]$, the elementary differential is defined recursively:

$$
F(\\tau)(y) = f^{(k)}(y)(F(\\tau_1)(y), \\dots, F(\\tau_k)(y))
$$

This connects the topological structure of the trees directly to the differential operators acting on the state space.
`;

export default function JSurface() {
  return (
    <div className="space-y-8">
      {/* Hero Section */}
      <div className="relative rounded-3xl overflow-hidden border border-border/50 bg-card/30 backdrop-blur-md">
        <div className="absolute inset-0 bg-[url('/images/j-surface-reactor.jpg')] bg-cover bg-center opacity-20 mix-blend-screen" />
        <div className="absolute inset-0 bg-gradient-to-r from-background via-background/80 to-transparent" />
        
        <div className="relative p-8 md:p-12 lg:p-16 max-w-3xl">
          <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full bg-primary/10 text-primary text-sm font-medium mb-6">
            <Layers className="h-4 w-4" />
            <span>Core Engine</span>
          </div>
          <h1 className="text-4xl md:text-5xl lg:text-6xl font-heading font-bold mb-6 tracking-tight">
            J-Surface Reactor
          </h1>
          <p className="text-lg md:text-xl text-muted-foreground leading-relaxed">
            A unified dynamical system where gradient flows meet evolutionary calculus on a symplectic manifold.
          </p>
        </div>
      </div>

      {/* Interactive Diagram Placeholder */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
        <div className="lg:col-span-2 space-y-8">
          <div className="prose prose-invert prose-headings:font-heading prose-headings:tracking-tight prose-p:text-muted-foreground prose-a:text-primary prose-code:text-primary prose-pre:bg-card/50 prose-pre:border prose-pre:border-border/50 max-w-none">
            <Streamdown>{content}</Streamdown>
          </div>
        </div>

        <div className="space-y-6">
          <Card className="border-border/50 bg-card/50 backdrop-blur-sm">
            <CardHeader>
              <CardTitle className="font-heading text-lg flex items-center gap-2">
                <Zap className="h-5 w-5 text-chart-3" />
                Key Properties
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-2">
                <h4 className="text-sm font-medium text-foreground">Symplectic Preservation</h4>
                <p className="text-sm text-muted-foreground">
                  Maintains $J^T J = -I$ throughout evolution, ensuring energy conservation.
                </p>
              </div>
              <div className="space-y-2">
                <h4 className="text-sm font-medium text-foreground">Recursive Differentials</h4>
                <p className="text-sm text-muted-foreground">
                  Computes $F(\tau)$ for trees up to order 8 using recursive directional derivatives.
                </p>
              </div>
              <div className="space-y-2">
                <h4 className="text-sm font-medium text-foreground">Ridge Coupling</h4>
                <p className="text-sm text-muted-foreground">
                  B-series ridges provide structural stability to the gradient flow dynamics.
                </p>
              </div>
            </CardContent>
          </Card>

          <Card className="border-border/50 bg-card/50 backdrop-blur-sm">
            <CardHeader>
              <CardTitle className="font-heading text-lg flex items-center gap-2">
                <GitBranch className="h-5 w-5 text-primary" />
                Tree Integration
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="relative aspect-square rounded-lg overflow-hidden border border-border/30">
                <div className="absolute inset-0 bg-gradient-to-br from-primary/20 to-chart-4/20 animate-pulse-slow" />
                <div className="absolute inset-0 flex items-center justify-center">
                  <div className="text-center space-y-2">
                    <div className="text-4xl font-bold text-foreground">A000081</div>
                    <div className="text-xs text-muted-foreground">Topology Source</div>
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  );
}
