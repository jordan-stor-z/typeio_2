CREATE VIEW project.project_vw AS
SELECT
  rn.project_id
  , rn.description
  , n.last_updated
  , rn.title
FROM
  project.node AS rn
  JOIN
  (  SELECT
        _n.project_id,
        MAX(_n.updated) AS last_updated
     FROM
       project.node AS _n
     GROUP BY
       _n.project_id
  ) AS n ON n.project_id = rn.project_id
WHERE
  rn.node_type_id = 'project_root'
