INSERT INTO Submission
	(id, title, upvoteCount, downvoteCount)
	VALUES
	(1, 'This is a picture of my cat', 3, 1);
INSERT INTO Submission
	(id, title, upvoteCount, downvoteCount)
	VALUES
	(2, 'DAE breathe?', 1, 2);

INSERT INTO Comment
	(id, content)
	VALUES
	(1, 'Cuteness overload');
INSERT INTO Comment
	(id, content)
	VALUES
	(2, 'You''re a kitty!');

INSERT INTO Submission_Comment
	(Submission_id, comments_id)
	VALUES
	(1, 1);
INSERT INTO Submission_Comment
	(Submission_id, comments_id)
	VALUES
	(1, 2);
