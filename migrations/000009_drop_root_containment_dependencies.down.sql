-- Recreates root-containment rows: one per non-root node, pointing at
-- its own project's root.
--
-- These are fully derivable from project.node.project_id, which is
-- precisely why they were redundant, and is what makes this reversible
-- without having kept a copy.
--
-- Note it restores the *canonical* set rather than byte-for-byte what
-- the up migration deleted. If some node was missing its row before --
-- created before Api.Node.Post started writing them, say -- this gives
-- it one. That is a superset, never a loss, and the rows carry no
-- information beyond the pairing itself.
INSERT INTO project.dependency (node_id, to_node_id)
SELECT n.id, r.id
FROM project.node n
JOIN project.node r
  ON r.project_id = n.project_id
 AND r.node_type_id = 'project_root'
WHERE n.node_type_id <> 'project_root'
ON CONFLICT (node_id, to_node_id) DO NOTHING;
