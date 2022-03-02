USE H_Accounting;

DROP PROCEDURE IF EXISTS `adiwan2019_sp`;

DELIMITER $$
	CREATE PROCEDURE `adiwan2019_sp`(varCalendarYear YEAR)
	BEGIN
		-- We receive as an argument the year for which we will calculate the balance sheet
    -- This value is stored as an 'YEAR' type in the variable `varCalendarYear`
    -- The procedure name will have to be changed to that of a new user, should another user run the code,
    -- as only the admin has rights view stored procedures for all user --
    
SELECT statement_section_order, statement_section_code, statement_section, 
	FORMAT(SUM((CASE WHEN YEAR(je.entry_date) = varCalendarYear THEN 
			(CASE WHEN ss.debit_is_positive = 0 THEN jel.credit ELSE jel.credit * -1 END)
			ELSE 0 END)),2) AS Amount
            FROM journal_entry_line_item AS jel
            INNER JOIN account AS ac ON ac.account_id = jel.account_id
            INNER JOIN journal_entry AS je ON je.journal_entry_id = jel.journal_entry_id
            INNER JOIN statement_section AS ss ON ss.statement_section_id = ac.profit_loss_section_id
            WHERE ac.profit_loss_section_id <> 0
            AND debit_credit_balanced <> 0
            AND cancelled = 0
            AND YEAR(je.entry_date) = varCalendarYear 
            GROUP BY statement_section_id;
END $$
DELIMITER ;

CALL adiwan2019_sp(2017);