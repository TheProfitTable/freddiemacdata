library(dplyr)
library(readr)

origfile_2012 <- read_delim("data/sample_orig_2012.txt", delim = "|", col_names = FALSE)
names(origfile_2012)=c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",'cnt_units','occpy_sts','cltv' ,'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name', 'flag_sc')

svcgfile_2012 <- read_delim("data/sample_svcg_2012.txt", delim = "|", col_names = FALSE)
names(svcgfile_2012)=c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age','mths_remng', 'repch_flag','flag_mod', 'cd_zero_bal', 'dt_zero_bal','current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries', 'net_sale_proceeds','non_mi_recoveries','expenses', 'legal_costs', 'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss', 'modcost')

# what do we need:
# origfile: dt_first_pi, flag_fthb, id_loan, channel, orig_loan_term
# svcgfile: id_loan, svcg_cycle, current_upb, delq_sts, loan_age, mi_recoveries, net_sale_proceeds, actual_loss
# missing: default_date, default_flag

# if lag < current and = defaultdefinition then default_date = pit_date

# create a seperate project for freddie mac transformation

df_default_test <- svcgfile_2012 %>%
  filter(id_loan == "F112Q1018643" | id_loan == "F112Q1033963") %>%
  group_by(id_loan) %>%
  mutate(lag_delq_sts = lag(delq_sts))

df_default_test_dd <- df_default_test %>%
  select(id_loan, svcg_cycle, delq_sts, lag_delq_sts) %>%
  mutate(default_date_all = if_else(lag_delq_sts < delq_sts & delq_sts == 3, svcg_cycle, as.integer(222201)))

df_default_test_dd_unique <- df_default_test_dd %>%
  group_by(id_loan) %>%
  summarise(default_date = min(default_date_all)) %>%
  mutate(default_date = if_else(default_date == 222201, 0, default_date))


