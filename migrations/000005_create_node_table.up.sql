CREATE TABLE project.node (
    attributes JSONB DEFAULT '{}'::jsonb,
    created TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted TIMESTAMPTZ NULL,
    description VARCHAR NOT NULL,
    id SERIAL PRIMARY KEY,
    node_status_id VARCHAR NOT NULL REFERENCES project.node_status(id),
    node_type_id VARCHAR NOT NULL REFERENCES project.node_type(id),
    project_id INT NOT NULL REFERENCES project.project(id),
    title VARCHAR NOT NULL,
    updated TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
