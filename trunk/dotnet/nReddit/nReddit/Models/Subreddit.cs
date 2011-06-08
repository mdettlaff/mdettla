using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Collections;

namespace nReddit.Models
{
    public class Subreddit
    {
        public Subreddit()
        {
            Submissions = new List<Submission>();
        }

        public int SubredditID { get; set; }
        public string Name { get; set; }
        public ICollection<Submission> Submissions { get; set; }
    }
}