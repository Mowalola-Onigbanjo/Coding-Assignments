USE H_Accounting;

DROP PROCEDURE IF EXISTS `monigbanjo2020_sp`;

DELIMITER $$
	CREATE PROCEDURE `monigbanjo2020_sp`(varCalendarYear YEAR)
	BEGIN
		-- We receive as an argument the year for which we will calculate the balance sheet
    -- This value is stored as an 'YEAR' type in the variable `varCalendarYear`
    -- The procedure name will have to be changed to that of a new user, should another user run the code,
    -- as only the admin has rights view stored procedures for all user --
    
SELECT statement_section_code, statement_section, 
			FORMAT(SUM(IFNULL(debit, 0)), 0) AS DEBIT, 
			FORMAT(SUM(IFNULL(credit, 0)), 0) AS CREDIT,  
			FORMAT(SUM(DEBIT) - SUM(CREDIT),0) AS Total
			FROM journal_entry_line_item AS jel
			INNER JOIN account AS ac ON ac.account_id = jel.account_id
			INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.balance_sheet_section_id
			INNER JOIN journal_entry AS je on je.journal_entry_id = jel.journal_entry_id
			WHERE balance_sheet_section_id <> 0
			AND  ss.statement_section_code IN ('CA', 'FA', 'DA', 'CL', 'LLL', 'DL', 'EQ')
			AND debit_credit_balanced <> 0
			AND YEAR(je.entry_date) <= varCalendarYear
			AND je.cancelled = 0
			AND is_balance_sheet_section = 1
			GROUP BY statement_section_id
			ORDER BY statement_section_id;
END $$
DELIMITER ;

CALL monigbanjo2020_sp(2017);
            
