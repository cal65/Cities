wb = openpyxl.load_workbook("Metros.xlsx")
sh = wb.get_active_sheet()
nrow=sh.max_row
values=[]
for i in range(1, nrow):
	sh.columns[1][i].value=sh.columns[1][i].value[1:] if sh.columns[1][i].value else 0
	values.append(sh.columns[1][i].value)

with open('citynames.csv', 'wb') as csvfile:
	writer=csv.writer(csvfile, delimiter=',')
	writer.writerow(values)