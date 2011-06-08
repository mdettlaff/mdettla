using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel;

namespace nReddit.Models
{
    public class Submission
    {
        public Submission()
        {
            Comments = new List<Comment>();
            UsernamesOfPeopleWhoVoted = "";
        }

        public int SubmissionID { get; set; }
        [Required(ErrorMessage = "Musisz podać URL")]
        [RegularExpression(@"^http://.+\..+",
            ErrorMessage = "Podany URL jest nieprawidłowy")]
        public string Url { get; set; }
        [Required(ErrorMessage = "Musisz podać tytuł")]
        [DisplayName("Tytuł")]
        [StringLength(100)]
        public string Title { get; set; }
        [DisplayName("Opis")]
        public string Text { get; set; }
        [ScaffoldColumn(false)]
        public int UpvoteCount { get; set; }
        [ScaffoldColumn(false)]
        public int DownvoteCount { get; set; }
        public ICollection<Comment> Comments { get; set; }
        public int SubredditID { get; set; }
        public Subreddit Subreddit { get; set; }
        public string Username { get; set; }
        [ScaffoldColumn(false)]
        public string UsernamesOfPeopleWhoVoted { get; set; }

        [ScaffoldColumn(false)]
        public int Score
        {
            get
            {
                return UpvoteCount - DownvoteCount;
            }
        }

        public void RememberVoter(string username)
        {
            UsernamesOfPeopleWhoVoted = UsernamesOfPeopleWhoVoted.Equals("")
                ? username : UsernamesOfPeopleWhoVoted + "," + username;
        }

        public bool UserAlreadyVoted(string username)
        {
            return UsernamesOfPeopleWhoVoted.Split(new char[] {','}).Contains(username);
        }

        public void Upvote()
        {
            UpvoteCount++;
        }

        public void Downvote()
        {
            DownvoteCount++;
        }
    }
}