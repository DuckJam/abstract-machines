import { useEffect, useRef, useState } from "react";
import { loadMachines, type Api, type Session, type View } from "./machines";

export default function App() {
  const [api, setApi] = useState<Api>();
  const [machine, setMachine] = useState("");
  const [source, setSource] = useState("(fun x -> x + 1) 2");
  const [error, setError] = useState<string | null>(null);
  const [view, setView] = useState<View>();
  const session = useRef<Session | null>(null);

  useEffect(() => {
    loadMachines().then((a) => { setApi(a); setMachine(a.machines[0]); });
  }, []);

  const load = () => {
    if (!api) return;
    const r = api.parse(source);
    if (!r.ok) { setError(r.error); return; }
    setError(null);
    session.current = api.create(machine, source);
    setView(session.current?.view());
  };
  useEffect(load, [api, machine]);   // restart when the machine changes

  const act = (f: (s: Session) => View) =>
    session.current && setView(f(session.current));

  if (!api) return <p>Loading…</p>;
  return (
    <main>
      <select value={machine} onChange={(e) => setMachine(e.target.value)}>
        {api.machines.map((m) => <option key={m}>{m}</option>)}
      </select>
      <textarea value={source} onChange={(e) => setSource(e.target.value)} />
      <button onClick={load}>Load</button>
      {error && <p style={{ color: "#b00020" }}>Parse error: {error}</p>}

      <button onClick={() => act((s) => s.step())}>Step</button>
      <button onClick={() => act((s) => s.back())}>Back</button>
      <button onClick={() => act((s) => s.run())}>Run</button>
      <button onClick={() => act((s) => s.reset())}>Reset</button>

      {view && (
        <>
          {view.registers.map((r) => (
            <section key={r.name}>
              <h3>{r.name}</h3>
              <ul>{r.items.map((it, i) => <li key={i}><code>{it}</code></li>)}</ul>
            </section>
          ))}
          <p>Step {view.steps}{view.final && " (final)"}</p>
          {view.error && <p>Machine error: {view.error}</p>}
        </>
      )}
    </main>
  );
}