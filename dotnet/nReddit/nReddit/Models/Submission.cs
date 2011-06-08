using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;

namespace nReddit.Models
{
    public class Submission
    {
        public int SubmissionID { get; set; }
        public string Url { get; set; }
        public string Title { get; set; }
        public string Text { get; set; }
        public int UpvoteCount { get; set; }
        public int DownvoteCount { get; set; }
        public ICollection<Comment> Comments { get; set; }
        public int SubredditID { get; set; }
        public Subreddit Subreddit { get; set; }

        public int Score
        {
            get
            {
                return UpvoteCount - DownvoteCount;
            }
        }
    }
}