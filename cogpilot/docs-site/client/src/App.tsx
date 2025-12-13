import { Toaster } from "@/components/ui/sonner";
import { TooltipProvider } from "@/components/ui/tooltip";
import NotFound from "@/pages/NotFound";
import { Route, Switch } from "wouter";
import ErrorBoundary from "./components/ErrorBoundary";
import Layout from "./components/Layout";
import { ThemeProvider } from "./contexts/ThemeContext";
import Home from "./pages/Home";
import JSurface from "./pages/JSurface";
import MembraneGardens from "./pages/MembraneGardens";
import OntogeneticEngine from "./pages/OntogeneticEngine";
import Evolution from "./pages/Evolution";

function Router() {
  return (
    <Layout>
      <Switch>
        <Route path="/" component={Home} />
        <Route path="/j-surface" component={JSurface} />
        <Route path="/membrane-gardens" component={MembraneGardens} />
        <Route path="/ontogenetic-engine" component={OntogeneticEngine} />
        <Route path="/evolution" component={Evolution} />
        <Route component={NotFound} />
      </Switch>
    </Layout>
  );
}

function App() {
  return (
    <ErrorBoundary>
      <ThemeProvider defaultTheme="dark">
        <TooltipProvider>
          <Toaster />
          <Router />
        </TooltipProvider>
      </ThemeProvider>
    </ErrorBoundary>
  );
}

export default App;
