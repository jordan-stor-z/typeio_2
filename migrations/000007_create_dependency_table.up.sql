CREATE TABLE project.dependency (
    id SERIAL PRIMARY KEY,
    node_id INT NOT NULL REFERENCES project.node(id),
    to_node_id INT NOT NULL REFERENCES project.node(id),
    CONSTRAINT unique_dependency UNIQUE (node_id, to_node_id)
);
