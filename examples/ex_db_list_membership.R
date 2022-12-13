
library(FIRVr)

filename <- "list_membership.db"

db_lm_create_list_membership_database(filename)

conn <- db_lm_connect(filename)

db_lm_create_list(
  conn,
  list_name =  "My List",
  list_desc = "A test list"
)

db_lm_update_list(
  conn,
  list_name =  "My List",
  list_desc = "A test list 2"
)

db_lm_create_security(
  conn,
  isin = "DE0001135176",
  security_des = "DE 2031",
  maturity_date = "2031-12-31"
)

db_lm_update_security(
  conn,
  isin = "DE0001135176",
  security_des = "DE 2031 NEW",
  maturity_date = "2031-11-30"
)

db_lm_create_membership(
  conn,
  list_name = "My List",
  isin = "DE0001135176",
  start_date = "2016-01-01",
  end_date = "2031-06-30"
)

db_lm_create_membership(
  conn,
  list_name = "My List",
  isin = "DE0001135176",
  start_date = "2018-01-01",
  end_date = "2031-06-30"
)

db_lm_update_membership(
  conn,
  list_name = "My List",
  isin = "DE0001135176",
  start_date = "2018-01-01",
  end_date = "2019-01-01"
)

list_membership <- db_lm_get_membership(
  conn,
  list_name = "My List",
  date = "2016-06-30"
)

print(list_membership)

print(db_lm_get_lists(conn))

print(db_lm_get_securities(conn))

db_lm_disconnect(conn)

file.remove(filename)
