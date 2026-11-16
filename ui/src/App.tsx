import { useEffect, useRef, useState } from "react";
import { loadMachines, type Api, type Session, type View } from "./machines";

export default function App() {
  const [api, setApi] = useState<Api>();
  const [machine, setMachine] = useState("");
  const [source, setSource] = useState("if 3 < 0 then (fun x -> x + 1) @ 2 else (fun x -> fun y -> y*x) @ 10 @ 3 ");
  const [error, setError] = useState<string | null>(null);
  const [view, setView] = useState<View>();
  const [prev, setPrev] = useState<View>();
  const show = (v: View) => { setPrev(view); setView(v); }
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

  const changed = (name: string, items: string[]) =>
  prev !== undefined &&
  JSON.stringify(prev.registers.find((r) => r.name === name)?.items) !==
    JSON.stringify(items);

  if (!api) return <p>Loading…</p>;
  return (
    <main className="app">
      <h1>Abstract machines</h1>

      <div className="toolbar">
        <label>
          Machine
          <select value={machine} onChange={(e) => setMachine(e.target.value)}>
            {api.machines.map((m) => <option key={m}>{m}</option>)}
          </select>
        </label>
      </div>

      <div className="editor">
        <textarea
          value={source}
          spellCheck={false}
          onChange={(e) => setSource(e.target.value)}
          onKeyDown={(e) => { if (e.ctrlKey && e.key === "Enter") load(); }}
        />
        <button className="primary" onClick={load}>Load</button>
      </div>
      {error && <p className="error" role="alert">Parse error: {error}</p>}

      <div className="controls">
        <button onClick={() => act((s) => s.back())}>Back</button>
        <button onClick={() => act((s) => s.step())} disabled={view?.final}>Step</button>
        <button onClick={() => act((s) => s.run())} disabled={view?.final}>Run</button>
        <button onClick={() => act((s) => s.reset())}>Reset</button>
        {view && (
          <span className="status">
            Step {view.steps}
            {view.final && <b className="final"> final</b>}
          </span>
        )}
      </div>
      {view?.error && <p className="error" role="alert">Machine error: {view.error}</p>}

      {view && (
        <div className="registers">
          {view.registers.map((r) => (
            <section
              key={`${r.name}-${view.steps}`}
              className={`register${changed(r.name, r.items) ? " changed" : ""}`}
              data-register={r.name}
            >
              <h3>{r.name}</h3>
              <ul>
                {r.items.map((it, i) => <li key={i}><code>{it}</code></li>)}
              </ul>
            </section>
          ))}
        </div>
      )}
    </main>
  );
}