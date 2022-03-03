SELECT p.person_id as subject_id, p.gender_concept_id,
  case when p.gender_concept_id = 8507 then 'Male'
  when p.gender_concept_id = 8532 then 'Female'
  ELSE 'Unknown' END as gender,
  DATEFROMPARTS(p.year_of_birth, ISNULL(p.month_of_birth, 7), ISNULL(p.day_of_birth, 1)) AS DOB,
  op.observation_period_start_date as COHORT_START_DATE,
  op.observation_period_end_date as COHORT_END_DATE,
  YEAR(op.observation_period_start_date) as start_year, YEAR(op.observation_period_end_date) as end_year,
  DATEFROMPARTS(YEAR(op.observation_period_start_date),MONTH(op.observation_period_start_date),1) as start_yearMth, DATEFROMPARTS(YEAR(op.observation_period_end_date),MONTH(op.observation_period_end_date),1) as end_yearMth
  FROM @cdm_database_schema.person p
  RIGHT JOIN @cdm_database_schema.observation_period op ON p.person_id = op.person_id
  WHERE YEAR(op.observation_period_start_date) <= @yearEndDate AND YEAR(op.observation_period_end_date) >= @yearStartDate
  AND p.year_of_birth IS NOT NULL
