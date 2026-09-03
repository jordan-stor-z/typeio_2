-- Membership was being stored twice (#198).
--
-- Api.Node.Post used to write a project.dependency row for every node
-- it created, pointing at the project root, to record "this node
-- belongs to this project". But project.node.project_id already
-- records that, so the row was duplicate data living in a table that
-- means something else: an ordering between two pieces of work.
--
-- The graph read those rows as real dependencies. Layering correctly
-- draws a dependent above what it waits on, so every node in the
-- project ended up above the root and the root sank to the bottom of
-- the drawing.
--
-- Only rows whose to_node_id is a project_root node are removed. A
-- dependency between two work nodes is a genuine ordering and is left
-- alone.
DELETE FROM project.dependency d
USING project.node n
WHERE d.to_node_id = n.id
  AND n.node_type_id = 'project_root';
