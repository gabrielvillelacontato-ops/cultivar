-- =====================================================================
-- CultivaR — Migration 003: Soft delete e autoria em workflows
-- =====================================================================
-- Versao do schema:  0.3.0
-- Data:              2026-07-02
--
-- Adiciona:
--   - workflows.deleted_at  (para soft delete, consistente com meios)
--   - workflows.criado_por  (rastreabilidade de autoria, consistente
--                            com meios.criado_por)
--
-- Motivacao:
--   Pesquisa exige flexibilidade: workflows evoluem, sao substituidos
--   ou abandonados, mas o historico precisa persistir para preservar
--   integridade dos preparos que os utilizaram.
-- =====================================================================

ALTER TABLE workflows ADD COLUMN deleted_at TEXT;

ALTER TABLE workflows ADD COLUMN criado_por INTEGER
    REFERENCES operadores(id);