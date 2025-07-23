CREATE TABLE project.node_status_change (
    id SERIAL PRIMARY KEY,
    node_id INT NOT NULL REFERENCES project.node(id),
    node_status_id VARCHAR NOT NULL REFERENCES project.node_status(id),
    created TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
