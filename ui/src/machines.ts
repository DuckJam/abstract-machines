export interface Register { name: string; items: string[] }
export interface View {
  registers: Register[]; final: boolean; error: string | null; steps: number;
}
export interface Session {
  step(): View; back(): View; run(): View; reset(): View; view(): View;
}
export interface Api {
  machines: string[];
  parse(src: string): { ok: true } | { ok: false; error: string };
  create(machine: string, src: string): Session | null;
}

interface RawSession {
  step(): string; back(): string; run(): string; reset(): string; view(): string;
}
interface RawApi {
  machines(): string[];
  parse(src: string): string;
  create(machine: string, src: string): RawSession | null;
}
declare global { interface Window { abstractMachines?: RawApi } }

const wrapSession = (r: RawSession): Session => ({
  step: () => JSON.parse(r.step()),
  back: () => JSON.parse(r.back()),
  run: () => JSON.parse(r.run()),
  reset: () => JSON.parse(r.reset()),
  view: () => JSON.parse(r.view()),
});

let loading: Promise<Api> | undefined;
export function loadMachines(): Promise<Api> {
  loading ??= new Promise<RawApi>((resolve, reject) => {
    const s = document.createElement("script");
    s.src = `${import.meta.env.BASE_URL}machines.js`;
    s.onload = () =>
      window.abstractMachines
        ? resolve(window.abstractMachines)
        : reject(new Error("machines.js loaded but exported nothing"));
    s.onerror = () => reject(new Error("could not load machines.js"));
    document.head.appendChild(s);
  }).then((raw): Api => ({
    machines: raw.machines(),
    parse: (src) => JSON.parse(raw.parse(src)),
    create: (m, src) => {
      const s = raw.create(m, src);
      return s ? wrapSession(s) : null;
    },
  }));
  return loading;
}