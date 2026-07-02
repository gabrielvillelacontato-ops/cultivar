## Drag-drop de etapas de workflow (v0.2)

**Tentativa inicial (v0.1):** integracao via `sortable::rank_list` +
`sortable::update_rank_list`. Funcional em fluxo isolado mas apresentou
dois bugs de dificil resolucao dentro do contexto de modulos Shiny + renderUI:

1. Apos reordenar via drag, os labels visuais nao renumeram sozinhos.
   O widget mantem estado interno que nao sincroniza com o banco mesmo
   apos `update_rank_list`.

2. Ao trocar entre workflows na tabela superior, o widget morre e
   nao volta a funcionar sem F5. O `renderUI` do painel principal
   recria o DOM parent, quebrando as referencias internas do sortable.

**Direcao para v0.2:**
- Substituir `renderUI` do painel principal por `bslib::navset_hidden`
  ou similar que preserve o DOM, e trocar o conteudo via `updateNavset`.
- OU forcar `css_id` dinamico + reset explicito do widget a cada
  troca de workflow via `session$sendCustomMessage`.
- OU migrar para JS custom com Shiny.setInputValue, contornando
  htmlwidgets/sortable.

**Preservado no codigo:**
- `sortable` como Imports no DESCRIPTION
- `R/zzz.R` com `.onLoad` chamando `sortable::enable_modules()`
- Helper `.wf_label_visual()` em `mod_workflows.R` (reutilizavel)

O MVP usa botoes moverpra cima/baixo (arrow up/down) em cada card
de etapa. Menos elegante, mas confiavel.