<script>
  let { status = "Starting up..." } = $props();
</script>

<div class="splash">
  <div class="splash-content">
    <img src="/marmot-logo.png" alt="MARMOT" class="splash-logo" />
    <div class="splash-title">MARMOT</div>
    <div class="splash-subtitle">Flow Cytometry Analysis</div>

    <div class="spinner-row">
      <span class="dot"></span>
      <span class="dot"></span>
      <span class="dot"></span>
    </div>

    <div class="splash-status">{status}</div>
  </div>

  <div class="splash-version">v0.4.0</div>
</div>

<style>
  .splash {
    position: fixed;
    inset: 0;
    background: linear-gradient(135deg, #0f172a 0%, #1e293b 60%, #334155 100%);
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    z-index: 1000;
  }

  .splash-content {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.6rem;
  }

  .splash-logo {
    width: 72px;
    height: 72px;
    border-radius: 14px;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5);
    margin-bottom: 0.4rem;
  }

  .splash-title {
    font-size: 2rem;
    font-weight: 800;
    letter-spacing: 0.18em;
    color: #dbeafe;
  }

  .splash-subtitle {
    font-size: 0.82rem;
    color: #94a3b8;
    letter-spacing: 0.04em;
    margin-top: -0.3rem;
  }

  .spinner-row {
    display: flex;
    gap: 0.45rem;
    margin-top: 1.4rem;
  }

  .dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: #3b82f6;
    animation: bounce 1.2s ease-in-out infinite;
  }
  .dot:nth-child(2) { animation-delay: 0.2s; }
  .dot:nth-child(3) { animation-delay: 0.4s; }

  @keyframes bounce {
    0%, 80%, 100% { transform: scale(0.7); opacity: 0.4; }
    40%            { transform: scale(1);   opacity: 1;   }
  }

  .splash-status {
    font-size: 0.75rem;
    color: #64748b;
    margin-top: 0.5rem;
    letter-spacing: 0.02em;
    min-height: 1.1em;
  }

  .splash-version {
    position: absolute;
    bottom: 1.2rem;
    font-size: 0.65rem;
    color: #334155;
    font-family: "SF Mono", "Fira Code", monospace;
    letter-spacing: 0.05em;
  }
</style>
