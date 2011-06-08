using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Entity;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using nReddit.Models;

namespace nReddit.Controllers
{ 
    public class SubmissionController : Controller
    {
        private NRedditEntities db = new NRedditEntities();

        //
        // GET: /Submission/Details/5

        public ViewResult Details(int id)
        {
            Submission submission = db.Submissions.Find(id);
            Subreddit subreddit = db.Subreddits.Find(submission.SubredditID);
            submission.Subreddit = subreddit;
            return View(submission);
        }

        //
        // GET: /Submission/Create

        [Authorize]
        public ActionResult Create()
        {
            return View();
        } 

        //
        // POST: /Submission/Create

        [HttpPost]
        [Authorize]
        public ActionResult Create(Submission submission)
        {
            if (ModelState.IsValid)
            {
                int subredditID = (int)Session["subredditID"];
                submission.SubredditID = subredditID;
                submission.Username = User.Identity.Name;
                Subreddit subreddit =
                    db.Subreddits.Include("Submissions").Single(s => s.SubredditID == subredditID);
                submission.Subreddit = subreddit;
                db.Submissions.Add(submission);
                subreddit.Submissions.Add(submission);
                db.Entry(subreddit).State = EntityState.Modified;
                db.SaveChanges();
                return RedirectToAction("Details", new { id = submission.SubmissionID });  
            }
            return View(submission);
        }
        
        //
        // GET: /Submission/Edit/5

        public ActionResult Edit(int id)
        {
            Submission submission = db.Submissions.Find(id);
            return View(submission);
        }

        [Authorize]
        [HttpPost]
        public ActionResult Upvote(int id)
        {
            Submission submission = db.Submissions.Find(id);
            string username = User.Identity.Name;
            if (submission.UserAlreadyVoted(username))
            {
                return redirectToVoteNotAllowed(submission);
            }
            submission.RememberVoter(username);
            submission.Upvote();
            db.Entry(submission).State = EntityState.Modified;
            db.SaveChanges();
            return RedirectToAction("Details", new { id = submission.SubmissionID });
        }

        [Authorize]
        [HttpPost]
        public ActionResult Downvote(int id)
        {
            Submission submission = db.Submissions.Find(id);
            string username = User.Identity.Name;
            if (submission.UserAlreadyVoted(username))
            {
                return redirectToVoteNotAllowed(submission);
            }
            submission.RememberVoter(username);
            submission.Downvote();
            db.Entry(submission).State = EntityState.Modified;
            db.SaveChanges();
            return RedirectToAction("Details", new { id = submission.SubmissionID });
        }

        private ActionResult redirectToVoteNotAllowed(Submission submission)
        {
            return RedirectToAction("Error", new
            {
                id = submission.SubmissionID,
                message = "Nie można głosować więcej niż raz na dany link"
            });
        }

        public ActionResult Error(int id, string message)
        {
            ViewBag.SubmissionID = id;
            ViewBag.Message = message;
            return View();
        }

        //
        // POST: /Submission/Edit/5

        [HttpPost]
        public ActionResult Edit(Submission submission)
        {
            if (ModelState.IsValid)
            {
                db.Entry(submission).State = EntityState.Modified;
                db.SaveChanges();
                return RedirectToAction("Details", new { id = submission.SubmissionID });
            }
            return View(submission);
        }

        //
        // GET: /Submission/Delete/5
 
        [Authorize]
        public ActionResult Delete(int id)
        {
            Submission submission = db.Submissions.Find(id);
            if (!authorizeCreator(submission))
            {
                return RedirectToAction("Error", new
                {
                    id = submission.SubmissionID,
                    message = "Można usuwać tylko własne linki"
                });
            }
            return View(submission);
        }

        //
        // POST: /Submission/Delete/5

        [Authorize]
        [HttpPost, ActionName("Delete")]
        public ActionResult DeleteConfirmed(int id)
        {            
            Submission submission = db.Submissions.Find(id);
            if (!authorizeCreator(submission))
            {
                return RedirectToAction("Error", new
                {
                    id = submission.SubmissionID,
                    message = "Można usuwać tylko własne linki"
                });
            }
            db.Submissions.Remove(submission);
            db.SaveChanges();
            return RedirectToAction("Details", "Subreddit", new { id = submission.SubredditID });
        }

        private bool authorizeCreator(Submission submission)
        {
            return User.Identity.Name.Equals(submission.Username) || User.IsInRole("Administrator");
        }

        public ActionResult SearchIndex(string searchString)
        {
            ViewBag.ShowMessage = false;
            var submissions = from s in db.Submissions select s;
            if (!String.IsNullOrEmpty(searchString))
            {
                submissions = submissions.Where(
                    s => s.Title.Contains(searchString) || s.Text.Contains(searchString));
                if (submissions.Count() == 0)
                {
                    ViewBag.ShowMessage = true;
                }
            }
            else
            {
                submissions = submissions.Take(0);
            }
            return View(submissions);
        }

        protected override void Dispose(bool disposing)
        {
            db.Dispose();
            base.Dispose(disposing);
        }
    }
}