import { Streamdown } from "streamdown";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Activity, Sprout, Network } from "lucide-react";

const content = `
# Membrane Gardens

**Membrane Gardens** are the cultivation grounds for rooted trees within the Deep Tree Echo system. They combine P-system membrane computing with reservoir dynamics to create a rich evolutionary environment.

## Architecture

The system consists of a hierarchical structure of membranes, each containing:
1. **Reservoir State**: A local echo state network
2. **Planted Trees**: Rooted trees that define the connectivity
3. **Evolution Rules**: Multiset rewriting rules for state transitions

### P-System Integration

The membrane evolution follows the equation:

$$
M_m(t+1) = M_m(t) + \\sum_{r \\in R} \\text{apply\\_rule}(r, M_m(t))
$$

Where $M_m(t)$ is the multiset of objects in membrane $m$ at time $t$, and $R$ is the set of evolution rules.

### Reservoir Coupling

The reservoir state $r_m(t)$ is coupled to the global J-surface state $\\psi$:

$$
r_m(t+1) = (1 - c \\cdot dt) \\cdot r_m(t) + c \\cdot dt \\cdot \\psi_{\\text{jsurface}}
$$

This creates a bidirectional feedback loop where the global dynamics influence local membrane evolution, and membrane states feed back into the global system.
`;

export default function MembraneGardens() {
  return (
    <div className="space-y-8">
      {/* Hero Section */}
      <div className="relative rounded-3xl overflow-hidden border border-border/50 bg-card/30 backdrop-blur-md">
        <div className="absolute inset-0 bg-[url('/images/membrane-garden.jpg')] bg-cover bg-center opacity-20 mix-blend-screen" />
        <div className="absolute inset-0 bg-gradient-to-r from-background via-background/80 to-transparent" />
        
        <div className="relative p-8 md:p-12 lg:p-16 max-w-3xl">
          <div className="inline-flex items-center gap-2 px-3 py-1 rounded-full bg-chart-2/10 text-chart-2 text-sm font-medium mb-6">
            <Activity className="h-4 w-4" />
            <span>Evolutionary Environment</span>
          </div>
          <h1 className="text-4xl md:text-5xl lg:text-6xl font-heading font-bold mb-6 tracking-tight">
            Membrane Gardens
          </h1>
          <p className="text-lg md:text-xl text-muted-foreground leading-relaxed">
            Hierarchical P-systems where rooted trees are cultivated, evolved, and coupled to the global reservoir dynamics.
          </p>
        </div>
      </div>

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
                <Sprout className="h-5 w-5 text-chart-1" />
                Cultivation Process
              </CardTitle>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex gap-4 items-start">
                <div className="flex-none w-6 h-6 rounded-full bg-chart-1/20 text-chart-1 flex items-center justify-center text-xs font-bold">1</div>
                <div>
                  <h4 className="text-sm font-medium text-foreground">Planting</h4>
                  <p className="text-sm text-muted-foreground">Trees generated from A000081 are planted in specific membranes.</p>
                </div>
              </div>
              <div className="flex gap-4 items-start">
                <div className="flex-none w-6 h-6 rounded-full bg-chart-1/20 text-chart-1 flex items-center justify-center text-xs font-bold">2</div>
                <div>
                  <h4 className="text-sm font-medium text-foreground">Growth</h4>
                  <p className="text-sm text-muted-foreground">Membrane rules evolve the local environment based on tree structure.</p>
                </div>
              </div>
              <div className="flex gap-4 items-start">
                <div className="flex-none w-6 h-6 rounded-full bg-chart-1/20 text-chart-1 flex items-center justify-center text-xs font-bold">3</div>
                <div>
                  <h4 className="text-sm font-medium text-foreground">Harvest</h4>
                  <p className="text-sm text-muted-foreground">Feedback from successful trees is integrated into the global system.</p>
                </div>
              </div>
            </CardContent>
          </Card>

          <Card className="border-border/50 bg-card/50 backdrop-blur-sm">
            <CardHeader>
              <CardTitle className="font-heading text-lg flex items-center gap-2">
                <Network className="h-5 w-5 text-chart-2" />
                Connectivity
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Membrane Depth</span>
                  <span className="font-mono">4 Levels</span>
                </div>
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Coupling Strength</span>
                  <span className="font-mono">Adaptive</span>
                </div>
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Feedback Rate</span>
                  <span className="font-mono">0.1 Hz</span>
                </div>
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  );
}
