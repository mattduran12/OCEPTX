-- Publish the four starter articles from article_metadata.csv.
INSERT INTO publications (article_id, title, authors, journal, article_type, publication_date, pdf_url, published)
VALUES
  ('CJ-2026-000', 'Make Time for the Shift Handoff', 'Dana Example', 'The Corrections Journal', 'Perspectives & Debate', '2026-11-27', 'articles/CJ-2026-000.pdf', 1),
  ('CS-2026-000', 'Closing the Loop After a Neighborhood Walk', 'Riley Placeholder', 'The Community Safety Journal', 'Research & Education', '2026-11-27', 'articles/CS-2026-000.pdf', 1),
  ('CT-2026-000', 'Writing Court Notices People Can Follow', 'Morgan Sample', 'The Courts Journal', 'Research & Education', '2026-11-27', 'articles/CT-2026-000.pdf', 1),
  ('PJ-2026-000', 'Small Changes, Safer Neighborhoods: A Fictional Case Study', 'Alex Example, Jordan Sample, Taylor Placeholder', 'The Police Journal', 'Practice & Experience', '2026-11-27', 'articles/PJ-2026-000.pdf', 1)
ON CONFLICT(article_id) DO UPDATE SET
  title = excluded.title,
  authors = excluded.authors,
  journal = excluded.journal,
  article_type = excluded.article_type,
  publication_date = excluded.publication_date,
  pdf_url = excluded.pdf_url,
  published = excluded.published;

-- Carry the old Police article's metrics over, including any clicks on the new ID.
INSERT INTO article_metrics (article_id, click_count, last_clicked_at)
SELECT 'PJ-2026-000', click_count, last_clicked_at
FROM article_metrics
WHERE article_id = 'PJ-2026-002'
ON CONFLICT(article_id) DO UPDATE SET
  click_count = article_metrics.click_count + excluded.click_count,
  last_clicked_at = NULLIF(MAX(
    COALESCE(article_metrics.last_clicked_at, ''),
    COALESCE(excluded.last_clicked_at, '')
  ), '');

DELETE FROM article_metrics WHERE article_id = 'PJ-2026-002';

-- Retain the previous publication record for reference and hide it from the archive.
UPDATE publications SET published = 0 WHERE article_id = 'PJ-2026-002';
