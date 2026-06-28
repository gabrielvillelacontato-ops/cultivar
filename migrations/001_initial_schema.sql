-- =====================================================================
-- CultivaR — Migration 001: Schema inicial
-- =====================================================================
-- Versao do schema:  0.1.0
-- Data:              2026-06-28
-- Autor:             Mauro Villela
-- Requer SQLite:     >= 3.7 (para WAL); RSQLite 2.3+ atende.
--
-- ESCOPO
-- Cria o esquema relacional completo do MVP do CultivaR. Idempotente
-- via IF NOT EXISTS. A funcao db_apply_migrations() em R registra
-- esta migration em schema_version apos execucao bem-sucedida.
--
-- CONVENCOES
--   * Toda tabela tenant-scoped tem coluna tenant_id (FK -> tenants).
--   * Timestamps em TEXT, formato ISO 8601 UTC.
--     SQLite 'now' retorna UTC por padrao (confirmado na doc).
--   * Datas (sem hora) em TEXT formato YYYY-MM-DD. Validacao em R.
--   * Soft delete via coluna deleted_at (NULL = ativo).
--   * Boolean como INTEGER com CHECK (0,1).
--   * Massa molar em g/mol; concentracao canonica em mg/L.
--   * Indices compostos comecam com tenant_id (multi-tenant futuro).
--   * FK enforcement requer PRAGMA foreign_keys=ON; aplicado por conexao
--     em R (db_connect()).
--   * JSON em audit_log validado em R antes de INSERT (evita dependencia
--     de json_valid() do SQLite 3.45+).
--
-- DECISOES EXPLICITAS DO MVP (revisao na v0.2)
--   * Sem versionamento de meios. Snapshot em preparo_componentes_usados.
--   * Sem rotacao de audit_log. Tabela cresce indefinidamente (retencao
--     ALCOA+).
--   * tenant_id replicado apenas no nivel de tabelas raiz (meios, preparos,
--     workflows). Tabelas-filhas herdam via FK + JOIN.
--   * codigo_curto e validade validados em R, nao em CHECK SQL
--     (SQLite sem regex nativo).
--   * atualizado_em em meios atualizado explicitamente em R; sem trigger.
-- =====================================================================

-- ---------------------------------------------------------------------
-- Controle de versao do schema
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS schema_version (
    version       TEXT PRIMARY KEY,
    applied_at    TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    description   TEXT
);

-- ---------------------------------------------------------------------
-- GRUPO 1 — Identidade (tenants e operadores)
-- ---------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tenants (
    id            INTEGER PRIMARY KEY,
    nome          TEXT NOT NULL UNIQUE,
    criado_em     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))
);

-- Operadores: PIN nunca armazenado em texto. Hash + salt via sodium em R.
-- Lockout: tentativas_falhas conta erros consecutivos; bloqueado_ate
-- guarda timestamp ate quando a conta esta travada.
CREATE TABLE IF NOT EXISTS operadores (
    id                  INTEGER PRIMARY KEY,
    tenant_id           INTEGER NOT NULL REFERENCES tenants(id),
    nome                TEXT NOT NULL,
    email               TEXT,
    pin_hash            BLOB NOT NULL,
    pin_salt            BLOB NOT NULL,
    papel               TEXT NOT NULL DEFAULT 'operador'
                          CHECK (papel IN ('operador','supervisor','admin')),
    ativo               INTEGER NOT NULL DEFAULT 1 CHECK (ativo IN (0,1)),
    tentativas_falhas   INTEGER NOT NULL DEFAULT 0
                          CHECK (tentativas_falhas >= 0 AND tentativas_falhas <= 99),
    bloqueado_ate       TEXT,
    criado_em           TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    deleted_at          TEXT,
    UNIQUE (tenant_id, nome)
);

CREATE INDEX IF NOT EXISTS idx_operadores_tenant_ativo
    ON operadores (tenant_id, ativo, deleted_at);

-- ---------------------------------------------------------------------
-- GRUPO 2 — Catalogo (categorias, componentes, POPs, meios)
-- ---------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS categorias_componente (
    id              INTEGER PRIMARY KEY,
    tenant_id       INTEGER NOT NULL REFERENCES tenants(id),
    nome            TEXT NOT NULL,
    ordem_exibicao  INTEGER NOT NULL DEFAULT 0,
    criado_em       TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    UNIQUE (tenant_id, nome)
);

CREATE TABLE IF NOT EXISTS categorias_meio (
    id              INTEGER PRIMARY KEY,
    tenant_id       INTEGER NOT NULL REFERENCES tenants(id),
    nome            TEXT NOT NULL,
    ordem_exibicao  INTEGER NOT NULL DEFAULT 0,
    criado_em       TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    UNIQUE (tenant_id, nome)
);

-- Componentes: MW e CAS opcionais (extratos complexos sem MW definida).
-- CAS sem UNIQUE para permitir sinonimos (mesmo CAS, nomes diferentes).
CREATE TABLE IF NOT EXISTS componentes (
    id            INTEGER PRIMARY KEY,
    tenant_id     INTEGER NOT NULL REFERENCES tenants(id),
    categoria_id  INTEGER NOT NULL REFERENCES categorias_componente(id),
    nome          TEXT NOT NULL,
    formula       TEXT,
    cas           TEXT,
    massa_molar   NUMERIC CHECK (massa_molar IS NULL OR massa_molar > 0),
    observacoes   TEXT,
    criado_em     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    UNIQUE (tenant_id, nome)
);

CREATE INDEX IF NOT EXISTS idx_componentes_tenant_categoria
    ON componentes (tenant_id, categoria_id);

-- POPs: referencia documental. url pode ser link externo (intranet)
-- ou path relativo a inst/extdata/pops/.
CREATE TABLE IF NOT EXISTS pops (
    id            INTEGER PRIMARY KEY,
    tenant_id     INTEGER NOT NULL REFERENCES tenants(id),
    codigo        TEXT NOT NULL,
    titulo        TEXT NOT NULL,
    versao        TEXT,
    url           TEXT NOT NULL,
    criado_em     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    UNIQUE (tenant_id, codigo)
);

-- Meios: flag_incerteza marca composicoes nao verificadas ou conflitantes.
-- codigo_curto: ASCII alfanumerico, usado em geracao de lote_interno.
--               Formato validado em R.
-- ph_alvo: 0-14 (limite quimicamente significativo).
-- atualizado_em: setado explicitamente em R em todo UPDATE.
CREATE TABLE IF NOT EXISTS meios (
    id                INTEGER PRIMARY KEY,
    tenant_id         INTEGER NOT NULL REFERENCES tenants(id),
    categoria_id      INTEGER NOT NULL REFERENCES categorias_meio(id),
    pop_id            INTEGER REFERENCES pops(id),
    nome              TEXT NOT NULL,
    codigo_curto      TEXT NOT NULL,
    referencia        TEXT,
    doi               TEXT,
    ph_alvo           NUMERIC CHECK (ph_alvo IS NULL OR (ph_alvo >= 0 AND ph_alvo <= 14)),
    observacoes       TEXT,
    flag_incerteza    INTEGER NOT NULL DEFAULT 0 CHECK (flag_incerteza IN (0,1)),
    nota_incerteza    TEXT,
    criado_por        INTEGER REFERENCES operadores(id),
    criado_em         TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    atualizado_em     TEXT,
    deleted_at        TEXT,
    UNIQUE (tenant_id, codigo_curto)
);

CREATE INDEX IF NOT EXISTS idx_meios_tenant_categoria_ativo
    ON meios (tenant_id, categoria_id, deleted_at);

-- meio_componentes: composicao do meio.
-- concentracao_mg_l: unidade canonica armazenada (cap em 1000000 mg/L
--                    = 1 kg/L, fisicamente impossivel em meio final).
-- valor_original + unidade_original: preservam fidelidade da fonte
--                    (ex: paper diz "BAP 5 uM", guardamos 5 + 'uM' e
--                    tambem 1.1262 mg/L).
CREATE TABLE IF NOT EXISTS meio_componentes (
    id                  INTEGER PRIMARY KEY,
    meio_id             INTEGER NOT NULL REFERENCES meios(id) ON DELETE CASCADE,
    componente_id       INTEGER NOT NULL REFERENCES componentes(id),
    concentracao_mg_l   NUMERIC NOT NULL
                          CHECK (concentracao_mg_l > 0 AND concentracao_mg_l <= 1000000),
    valor_original      NUMERIC CHECK (valor_original IS NULL OR valor_original > 0),
    unidade_original    TEXT
                          CHECK (unidade_original IS NULL OR unidade_original IN
                                 ('mg/L','ug/L','g/L','ug/mL','mg/mL',
                                  'uM','nM','mM','M','%','ppm','x')),
    ordem_exibicao      INTEGER NOT NULL DEFAULT 0,
    observacao          TEXT,
    UNIQUE (meio_id, componente_id)
);

CREATE INDEX IF NOT EXISTS idx_meio_componentes_meio
    ON meio_componentes (meio_id);

CREATE INDEX IF NOT EXISTS idx_meio_componentes_componente
    ON meio_componentes (componente_id);

-- ---------------------------------------------------------------------
-- GRUPO 3 — Workflows (pipelines de meios)
-- ---------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS workflows (
    id            INTEGER PRIMARY KEY,
    tenant_id     INTEGER NOT NULL REFERENCES tenants(id),
    nome          TEXT NOT NULL,
    referencia    TEXT,
    doi           TEXT,
    descricao     TEXT,
    criado_em     TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    UNIQUE (tenant_id, nome)
);

-- workflow_etapas: duracao em texto livre (informativo, nao calculado).
CREATE TABLE IF NOT EXISTS workflow_etapas (
    id            INTEGER PRIMARY KEY,
    workflow_id   INTEGER NOT NULL REFERENCES workflows(id) ON DELETE CASCADE,
    meio_id       INTEGER NOT NULL REFERENCES meios(id),
    ordem         INTEGER NOT NULL CHECK (ordem >= 1),
    nome_etapa    TEXT NOT NULL,
    duracao       TEXT,
    condicoes     TEXT,
    observacoes   TEXT,
    UNIQUE (workflow_id, ordem)
);

CREATE INDEX IF NOT EXISTS idx_workflow_etapas_workflow_ordem
    ON workflow_etapas (workflow_id, ordem);

-- ---------------------------------------------------------------------
-- GRUPO 4 — Estoque fisico (lotes de reagentes)
-- ---------------------------------------------------------------------

-- reagentes_lotes: cada frasco fisico que entra no lab.
-- validade: YYYY-MM-DD, validado em R.
-- solvente + concentracao_estoque_mg_ml: descrevem estoques preparados
--   (ex: acetosiringona 100 mg/mL em DMSO).
CREATE TABLE IF NOT EXISTS reagentes_lotes (
    id                          INTEGER PRIMARY KEY,
    tenant_id                   INTEGER NOT NULL REFERENCES tenants(id),
    componente_id               INTEGER NOT NULL REFERENCES componentes(id),
    fabricante                  TEXT,
    lote_fabricante             TEXT,
    catalogo                    TEXT,
    data_recebimento            TEXT,
    validade                    TEXT,
    solvente                    TEXT,
    concentracao_estoque_mg_ml  NUMERIC
                                  CHECK (concentracao_estoque_mg_ml IS NULL
                                         OR concentracao_estoque_mg_ml > 0),
    quantidade_inicial          NUMERIC
                                  CHECK (quantidade_inicial IS NULL
                                         OR quantidade_inicial > 0),
    unidade_quantidade          TEXT,
    observacoes                 TEXT,
    ativo                       INTEGER NOT NULL DEFAULT 1 CHECK (ativo IN (0,1)),
    criado_em                   TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))
);

CREATE INDEX IF NOT EXISTS idx_reagentes_lotes_tenant_componente_ativo
    ON reagentes_lotes (tenant_id, componente_id, ativo);

CREATE INDEX IF NOT EXISTS idx_reagentes_lotes_validade
    ON reagentes_lotes (tenant_id, validade);

-- ---------------------------------------------------------------------
-- GRUPO 5 — Preparos (execucao de meio)
-- ---------------------------------------------------------------------

-- preparos: lote_interno gerado em R como <codigo_curto>_<YYYY-MM-DD>_<seq>.
-- Race condition em sequencial mitigada via retry em R (UNIQUE garante).
-- volume_final_ml cap em 1000000 mL (1000 L; preparo industrial extremo).
-- ph_medido 0-14 (qualquer valor fora indica erro de leitura).
-- status permite rascunho persistente (resiliencia a interrupcao).
CREATE TABLE IF NOT EXISTS preparos (
    id                  INTEGER PRIMARY KEY,
    tenant_id           INTEGER NOT NULL REFERENCES tenants(id),
    meio_id             INTEGER NOT NULL REFERENCES meios(id),
    operador_id         INTEGER NOT NULL REFERENCES operadores(id),
    lote_interno        TEXT NOT NULL,
    volume_final_ml     NUMERIC NOT NULL
                          CHECK (volume_final_ml > 0 AND volume_final_ml <= 1000000),
    ph_medido           NUMERIC
                          CHECK (ph_medido IS NULL OR (ph_medido >= 0 AND ph_medido <= 14)),
    status              TEXT NOT NULL DEFAULT 'rascunho'
                          CHECK (status IN ('rascunho','em_preparo','concluido','descartado')),
    iniciado_em         TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    concluido_em        TEXT,
    motivo_descarte     TEXT,
    observacoes         TEXT,
    UNIQUE (tenant_id, lote_interno)
);

CREATE INDEX IF NOT EXISTS idx_preparos_tenant_operador_data
    ON preparos (tenant_id, operador_id, iniciado_em);

CREATE INDEX IF NOT EXISTS idx_preparos_tenant_meio_status
    ON preparos (tenant_id, meio_id, status);

-- preparo_componentes_usados: snapshot do que foi pesado.
-- massa_pesada_mg pode diferir ligeiramente de massa_teorica_mg
-- (arredondamento de balanca).
CREATE TABLE IF NOT EXISTS preparo_componentes_usados (
    id                       INTEGER PRIMARY KEY,
    preparo_id               INTEGER NOT NULL REFERENCES preparos(id) ON DELETE CASCADE,
    componente_id            INTEGER NOT NULL REFERENCES componentes(id),
    reagente_lote_id         INTEGER REFERENCES reagentes_lotes(id),
    concentracao_alvo_mg_l   NUMERIC NOT NULL CHECK (concentracao_alvo_mg_l > 0),
    massa_teorica_mg         NUMERIC NOT NULL CHECK (massa_teorica_mg > 0),
    massa_pesada_mg          NUMERIC CHECK (massa_pesada_mg IS NULL OR massa_pesada_mg > 0),
    observacao               TEXT
);

CREATE INDEX IF NOT EXISTS idx_preparo_comp_usados_preparo
    ON preparo_componentes_usados (preparo_id);

CREATE INDEX IF NOT EXISTS idx_preparo_comp_usados_componente
    ON preparo_componentes_usados (componente_id);

-- ---------------------------------------------------------------------
-- GRUPO 6 — Auditoria (append-only por convencao)
-- ---------------------------------------------------------------------

-- audit_log: append-only por convencao da aplicacao (sem trigger SQL).
-- Toda escrita no sistema gera registro aqui na mesma transacao.
-- valores_antes/depois em JSON (TEXT) — validado em R antes de INSERT.
-- operador_id NULL valido para LOGIN_FALHO com usuario inexistente.
CREATE TABLE IF NOT EXISTS audit_log (
    id                INTEGER PRIMARY KEY,
    tenant_id         INTEGER NOT NULL REFERENCES tenants(id),
    operador_id       INTEGER REFERENCES operadores(id),
    timestamp         TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
    entidade_tabela   TEXT NOT NULL,
    entidade_id       INTEGER,
    acao              TEXT NOT NULL
                        CHECK (acao IN ('INSERT','UPDATE','DELETE','RESTAURAR',
                                        'LOGIN','LOGIN_FALHO','LOGOUT','LOCKOUT')),
    valores_antes     TEXT,
    valores_depois    TEXT,
    contexto          TEXT
);

CREATE INDEX IF NOT EXISTS idx_audit_tenant_timestamp
    ON audit_log (tenant_id, timestamp);

CREATE INDEX IF NOT EXISTS idx_audit_entidade
    ON audit_log (entidade_tabela, entidade_id);

CREATE INDEX IF NOT EXISTS idx_audit_operador
    ON audit_log (tenant_id, operador_id, timestamp);

-- ---------------------------------------------------------------------
-- Seeds minimos (idempotentes via INSERT OR IGNORE)
-- ---------------------------------------------------------------------

-- Tenant default. ID=1 fixo para simplificar o MVP.
-- Nome ajustavel posteriormente sem afetar referencias.
INSERT OR IGNORE INTO tenants (id, nome) VALUES (1, 'Padrao');



-- =====================================================================
-- Fim da migration 001
-- =====================================================================