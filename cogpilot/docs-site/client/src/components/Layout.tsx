import { Link, useLocation } from "wouter";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet";
import { Menu, TreeDeciduous, Activity, Layers, Network, GitBranch } from "lucide-react";

const navItems = [
  { name: "Overview", path: "/", icon: TreeDeciduous },
  { name: "J-Surface Reactor", path: "/j-surface", icon: Layers },
  { name: "Membrane Gardens", path: "/membrane-gardens", icon: Activity },
  { name: "Ontogenetic Engine", path: "/ontogenetic-engine", icon: Network },
  { name: "Evolution Summary", path: "/evolution", icon: GitBranch },
];

export default function Layout({ children }: { children: React.ReactNode }) {
  const [location] = useLocation();

  return (
    <div className="min-h-screen bg-background font-sans text-foreground selection:bg-primary/20 selection:text-primary">
      {/* Background Elements */}
      <div className="fixed inset-0 z-0 pointer-events-none overflow-hidden">
        <div className="absolute top-0 left-0 w-full h-full bg-[url('/images/hero-background.jpg')] bg-cover bg-center opacity-5 mix-blend-screen" />
        <div className="absolute inset-0 bg-gradient-to-b from-background via-background/95 to-background" />
        <div className="absolute top-0 left-0 w-full h-px bg-gradient-to-r from-transparent via-primary/20 to-transparent" />
      </div>

      <div className="relative z-10 flex flex-col min-h-screen">
        {/* Header */}
        <header className="sticky top-0 z-50 w-full border-b border-border/40 bg-background/80 backdrop-blur-md supports-[backdrop-filter]:bg-background/60">
          <div className="container flex h-16 items-center justify-between">
            <div className="flex items-center gap-2">
              <Sheet>
                <SheetTrigger asChild>
                  <Button variant="ghost" size="icon" className="md:hidden text-muted-foreground hover:text-primary">
                    <Menu className="h-5 w-5" />
                    <span className="sr-only">Toggle navigation menu</span>
                  </Button>
                </SheetTrigger>
                <SheetContent side="left" className="w-64 border-r border-border/40 bg-background/95 backdrop-blur-xl">
                  <div className="flex flex-col gap-6 py-4">
                    <Link href="/" className="flex items-center gap-2 font-heading font-bold text-xl tracking-tight text-primary">
                      <TreeDeciduous className="h-6 w-6" />
                      <span>Deep Tree Echo</span>
                    </Link>
                    <nav className="flex flex-col gap-1">
                      {navItems.map((item) => (
                        <Link key={item.path} href={item.path}>
                          <Button
                            variant="ghost"
                            className={cn(
                              "w-full justify-start gap-3 font-medium transition-all duration-200",
                              location === item.path 
                                ? "bg-primary/10 text-primary shadow-[0_0_10px_-5px_var(--primary)]" 
                                : "text-muted-foreground hover:text-foreground hover:bg-muted/50"
                            )}
                          >
                            <item.icon className="h-4 w-4" />
                            {item.name}
                          </Button>
                        </Link>
                      ))}
                    </nav>
                  </div>
                </SheetContent>
              </Sheet>
              
              <Link href="/" className="flex items-center gap-3 transition-opacity hover:opacity-80">
                <div className="relative flex h-8 w-8 items-center justify-center rounded-lg bg-primary/10 ring-1 ring-primary/20">
                  <TreeDeciduous className="h-5 w-5 text-primary" />
                </div>
                <span className="hidden font-heading font-bold text-lg tracking-tight md:inline-block bg-clip-text text-transparent bg-gradient-to-r from-foreground to-foreground/70">
                  Deep Tree Echo
                </span>
              </Link>
            </div>

            <nav className="hidden md:flex items-center gap-1">
              {navItems.map((item) => (
                <Link key={item.path} href={item.path}>
                  <Button
                    variant="ghost"
                    size="sm"
                    className={cn(
                      "gap-2 transition-all duration-300",
                      location === item.path 
                        ? "text-primary bg-primary/5 ring-1 ring-primary/20" 
                        : "text-muted-foreground hover:text-foreground hover:bg-muted/30"
                    )}
                  >
                    <item.icon className="h-4 w-4" />
                    {item.name}
                  </Button>
                </Link>
              ))}
            </nav>

            <div className="flex items-center gap-2">
              <Button variant="outline" size="sm" className="hidden sm:flex gap-2 border-primary/20 hover:border-primary/50 hover:bg-primary/5 text-primary/80 hover:text-primary transition-all duration-300">
                <GitBranch className="h-4 w-4" />
                <span>v2.0.0</span>
              </Button>
            </div>
          </div>
        </header>

        {/* Main Content */}
        <main className="flex-1 container py-8 md:py-12 lg:py-16 animate-in fade-in duration-500 slide-in-from-bottom-4">
          {children}
        </main>

        {/* Footer */}
        <footer className="border-t border-border/40 bg-background/50 backdrop-blur-sm">
          <div className="container flex flex-col items-center justify-between gap-4 py-10 md:h-24 md:flex-row md:py-0">
            <div className="flex flex-col items-center gap-4 px-8 md:flex-row md:gap-2 md:px-0">
              <TreeDeciduous className="h-6 w-6 text-primary/50" />
              <p className="text-center text-sm leading-loose text-muted-foreground md:text-left">
                Built by <span className="font-medium text-foreground">Manus AI</span>. 
                Based on the <span className="font-medium text-foreground">Deep Tree Echo</span> architecture.
              </p>
            </div>
            <div className="flex items-center gap-4 text-sm text-muted-foreground">
              <span>OEIS A000081</span>
              <span className="h-4 w-px bg-border" />
              <span>Julia Lang</span>
            </div>
          </div>
        </footer>
      </div>
    </div>
  );
}
