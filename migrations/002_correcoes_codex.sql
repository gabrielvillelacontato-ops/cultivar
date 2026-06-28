-- =====================================================================
-- CultivaR — Migration 002: Correcoes apos revisao Codex (2026-06-29)
-- =====================================================================
-- Versao do schema:  0.2.0
-- Data:              2026-06-29
-- Autor:             Mauro Villela
--
-- ESCOPO
-- Aplica correcoes identificadas na revisao critica do Codex:
--   1. Adiciona coluna meios.bloqueado_preparo (bloqueia PIA ate
--      validar AB salts e tampao fosfato).
--   2. Renomeia componente Na2EDTA -> Na2EDTA.2H2O (CAS 6381-92-6
--      e MW 372.24 correspondem ao dihidratado).
--   3. Insere componente Fe-Na-EDTA (CAS 15708-41-5, MW 367.1) como
--      forma comercial padrao para meios.
--   4. Substitui composicoes: troca o par FeSO4.7H2O + Na2EDTA por
--      Fe-Na-EDTA em todos os meios MS/B5 derivados.
--   5. Corrige B5: CuSO4 anidro -> CuSO4.5H2O.
--   6. Corrige PIA: MES 1463 -> 1464.3 mg/L (7.5 mM x 195.24 g/mol).
--   7. Marca PIA como bloqueado_preparo = 1.
--
-- CONVENCOES
--   * Idempotente: rodar duas vezes nao duplica nem altera valores.
--   * Tudo em transacao unica (BEGIN/COMMIT implicitos via
--     db_apply_migrations em R).
--   * DELETE+INSERT em meio_componentes para trocar componente_id
--     (UNIQUE(meio_id, componente_id) impede UPDATE direto).
--
-- REQUER SQLite >= 3.35 (ALTER TABLE ADD COLUMN com default).
-- =====================================================================

-- ---------------------------------------------------------------------
-- 1. Adiciona coluna bloqueado_preparo
-- ---------------------------------------------------------------------
ALTER TABLE meios ADD COLUMN bloqueado_preparo INTEGER NOT NULL DEFAULT 0
    CHECK (bloqueado_preparo IN (0,1));

-- ---------------------------------------------------------------------
-- 2. Renomeia Na2EDTA -> Na2EDTA.2H2O
-- ---------------------------------------------------------------------
UPDATE componentes
SET nome = 'Na2EDTA.2H2O',
    formula = 'Na2EDTA.2H2O',
    observacoes = 'CAS 6381-92-6 e MW 372.24 correspondem ao sal dissodico dihidratado'
WHERE tenant_id = 1
  AND nome = 'Na2EDTA';

-- ---------------------------------------------------------------------
-- 3. Insere Fe-Na-EDTA como componente
-- ---------------------------------------------------------------------
INSERT INTO componentes
    (tenant_id, categoria_id, nome, formula, cas, massa_molar, observacoes)
SELECT 1,
       (SELECT id FROM categorias_componente WHERE tenant_id = 1 AND nome = 'micronutriente'),
       'Fe-Na-EDTA',
       'C10H12N2NaFeO8',
       '15708-41-5',
       367.1,
       'Forma anidra. Sigma vende como "hydrate, 12-14% Fe basis" - composicao variavel por lote. Para precisao por lote, usar concentracao_estoque_mg_ml em reagentes_lotes.'
WHERE NOT EXISTS (
    SELECT 1 FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA'
);

-- ---------------------------------------------------------------------
-- 4. Substitui FeSO4.7H2O + Na2EDTA.2H2O por Fe-Na-EDTA nos meios MS/B5
-- ---------------------------------------------------------------------
-- Para cada meio afetado: DELETE das duas linhas antigas, INSERT da nova.

-- 4a. Meios com Fe-EDTA em concentracao padrao (36.71 mg/L equivalente
--     a 0.1 mmol/L de Fe, mesma molaridade de 27.8 mg/L FeSO4.7H2O)
DELETE FROM meio_componentes
WHERE meio_id IN (
    SELECT id FROM meios WHERE tenant_id = 1
      AND codigo_curto IN ('MSR','MS0','P1AS','P1S','P7M','MSBOK')
)
AND componente_id IN (
    SELECT id FROM componentes WHERE tenant_id = 1
      AND nome IN ('FeSO4.7H2O','Na2EDTA.2H2O','Na2EDTA')
);

INSERT INTO meio_componentes
    (meio_id, componente_id, concentracao_mg_l, valor_original, unidade_original,
     ordem_exibicao, observacao)
SELECT
    m.id,
    (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA'),
    36.71,
    NULL,
    NULL,
    6,
    'Substitui o par FeSO4.7H2O 27.8 + Na2EDTA 37.3 (mesma molaridade de Fe: 0.1 mmol/L).'
FROM meios m
WHERE m.tenant_id = 1
  AND m.codigo_curto IN ('MSR','MS0','P1AS','P1S','P7M','MSBOK')
  AND NOT EXISTS (
    SELECT 1 FROM meio_componentes mc
    WHERE mc.meio_id = m.id
      AND mc.componente_id = (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA')
  );

-- 4b. Meios em 1/2x (EG3, MS3): concentracao 18.355 mg/L
DELETE FROM meio_componentes
WHERE meio_id IN (
    SELECT id FROM meios WHERE tenant_id = 1
      AND codigo_curto IN ('EG3','MS3')
)
AND componente_id IN (
    SELECT id FROM componentes WHERE tenant_id = 1
      AND nome IN ('FeSO4.7H2O','Na2EDTA.2H2O','Na2EDTA')
);

INSERT INTO meio_componentes
    (meio_id, componente_id, concentracao_mg_l, valor_original, unidade_original,
     ordem_exibicao, observacao)
SELECT
    m.id,
    (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA'),
    18.355,
    NULL,
    NULL,
    6,
    'MS Revised salts a 1/2x. Substitui o par FeSO4.7H2O 13.9 + Na2EDTA 18.65.'
FROM meios m
WHERE m.tenant_id = 1
  AND m.codigo_curto IN ('EG3','MS3')
  AND NOT EXISTS (
    SELECT 1 FROM meio_componentes mc
    WHERE mc.meio_id = m.id
      AND mc.componente_id = (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA')
  );

-- 4c. B5: mantem mesma concentracao 36.71 (idem padrao MS)
DELETE FROM meio_componentes
WHERE meio_id IN (
    SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'B5'
)
AND componente_id IN (
    SELECT id FROM componentes WHERE tenant_id = 1
      AND nome IN ('FeSO4.7H2O','Na2EDTA.2H2O','Na2EDTA')
);

INSERT INTO meio_componentes
    (meio_id, componente_id, concentracao_mg_l, valor_original, unidade_original,
     ordem_exibicao, observacao)
SELECT
    m.id,
    (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA'),
    36.71,
    NULL,
    NULL,
    6,
    'B5 usa Sequestrene 330 Fe 28 mg/L no paper original. Fe-Na-EDTA pronto e equivalente comercial moderno.'
FROM meios m
WHERE m.tenant_id = 1
  AND m.codigo_curto = 'B5'
  AND NOT EXISTS (
    SELECT 1 FROM meio_componentes mc
    WHERE mc.meio_id = m.id
      AND mc.componente_id = (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA')
  );

-- 4d. MSO (basal original 1962): valor do paper era NaFe-EDTA 25 mg/L
DELETE FROM meio_componentes
WHERE meio_id IN (
    SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'MSO'
)
AND componente_id IN (
    SELECT id FROM componentes WHERE tenant_id = 1
      AND nome IN ('FeSO4.7H2O','Na2EDTA.2H2O','Na2EDTA')
);

INSERT INTO meio_componentes
    (meio_id, componente_id, concentracao_mg_l, valor_original, unidade_original,
     ordem_exibicao, observacao)
SELECT
    m.id,
    (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA'),
    25,
    NULL,
    NULL,
    8,
    'Valor do paper original M&S 1962 (NaFe-EDTA 25 mg/L).'
FROM meios m
WHERE m.tenant_id = 1
  AND m.codigo_curto = 'MSO'
  AND NOT EXISTS (
    SELECT 1 FROM meio_componentes mc
    WHERE mc.meio_id = m.id
      AND mc.componente_id = (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA')
  );

-- ---------------------------------------------------------------------
-- 5. Corrige B5: CuSO4 anidro -> CuSO4.5H2O
-- ---------------------------------------------------------------------
DELETE FROM meio_componentes
WHERE meio_id = (SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'B5')
  AND componente_id = (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'CuSO4 anidro');

INSERT INTO meio_componentes
    (meio_id, componente_id, concentracao_mg_l, valor_original, unidade_original,
     ordem_exibicao, observacao)
SELECT
    (SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'B5'),
    (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'CuSO4.5H2O'),
    0.025,
    NULL,
    NULL,
    13,
    'Corrigido Codex 2026-06-29: paper original e referencias comerciais usam pentahidratado.'
WHERE NOT EXISTS (
    SELECT 1 FROM meio_componentes
    WHERE meio_id = (SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'B5')
      AND componente_id = (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'CuSO4.5H2O')
);

-- ---------------------------------------------------------------------
-- 6. Corrige PIA: MES 1463 -> 1464.3 mg/L
-- ---------------------------------------------------------------------
UPDATE meio_componentes
SET concentracao_mg_l = 1464.3,
    observacao = 'Corrigido Codex 2026-06-29: 7.5 mM x 195.24 g/mol = 1464.3 mg/L (era 1463).'
WHERE meio_id = (SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'PIA')
  AND componente_id = (SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'MES')
  AND concentracao_mg_l = 1463;

-- ---------------------------------------------------------------------
-- 7. Bloqueia PIA para preparo (AB salts e tampao fosfato pendentes)
-- ---------------------------------------------------------------------
UPDATE meios
SET bloqueado_preparo = 1,
    nota_incerteza = 'BLOQUEADO PARA PREPARO. AB salts (Chilton et al. 1974) e tampao fosfato 2 mM (pH 5.6) PENDENTES de validacao bibliografica. Consultar paper original Sunilkumar & Rathore 2001 (Mol Breeding 8:37-52) ou Chilton et al. 1974 antes de remover bloqueio.'
WHERE tenant_id = 1
  AND codigo_curto = 'PIA';

-- =====================================================================
-- Fim da migration 002
-- =====================================================================