--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: log_doc_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_test
--

CREATE FUNCTION log_doc_event_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
            INSERT INTO doc_event (ts, doc, blessed_version) SELECT now(), NEW.id, NEW.current_version;
            RETURN NEW;
        END IF;
        RETURN NULL;
    END;
$$;


ALTER FUNCTION public.log_doc_event_trigger() OWNER TO snowdrift_test;

--
-- Name: log_role_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_test
--

CREATE FUNCTION log_role_event_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF (TG_OP = 'DELETE') THEN
            INSERT INTO role_event (ts, "user", role, project, added) SELECT now(), OLD."user", OLD.role, OLD.project, 'f';
            RETURN OLD;
        ELSIF (TG_OP = 'INSERT') THEN
            INSERT INTO role_event (ts, "user", role, project, added) SELECT now(), NEW."user", NEW.role, NEW.project, 't';
            RETURN NEW;
        END IF;
        RETURN NULL;
    END;
$$;


ALTER FUNCTION public.log_role_event_trigger() OWNER TO snowdrift_test;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: a; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE a (
    id integer NOT NULL
);


ALTER TABLE public.a OWNER TO snowdrift_test;

--
-- Name: a_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE a_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.a_id_seq OWNER TO snowdrift_test;

--
-- Name: a_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE a_id_seq OWNED BY a.id;


--
-- Name: account; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE account (
    id integer NOT NULL,
    balance bigint NOT NULL
);


ALTER TABLE public.account OWNER TO snowdrift_test;

--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.account_id_seq OWNER TO snowdrift_test;

--
-- Name: account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE account_id_seq OWNED BY account.id;


--
-- Name: blog_post; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE blog_post (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    title character varying NOT NULL,
    "user" bigint NOT NULL,
    top_content character varying NOT NULL,
    project bigint NOT NULL,
    bottom_content character varying,
    discussion bigint NOT NULL,
    handle character varying NOT NULL
);


ALTER TABLE public.blog_post OWNER TO snowdrift_test;

--
-- Name: build; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE build (
    id integer NOT NULL,
    boot_time timestamp with time zone NOT NULL,
    base character varying NOT NULL,
    diff character varying NOT NULL
);


ALTER TABLE public.build OWNER TO snowdrift_test;

--
-- Name: build_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE build_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.build_id_seq OWNER TO snowdrift_test;

--
-- Name: build_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE build_id_seq OWNED BY build.id;


--
-- Name: comment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    approved_ts timestamp with time zone,
    approved_by bigint,
    parent bigint,
    "user" bigint NOT NULL,
    text character varying NOT NULL,
    depth bigint NOT NULL,
    discussion bigint NOT NULL,
    visibility character varying DEFAULT 'VisPublic'::character varying NOT NULL,
    language character varying NOT NULL
);


ALTER TABLE public.comment OWNER TO snowdrift_test;

--
-- Name: comment_ancestor; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_ancestor (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ancestor bigint NOT NULL
);


ALTER TABLE public.comment_ancestor OWNER TO snowdrift_test;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_ancestor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_ancestor_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_ancestor_id_seq OWNED BY comment_ancestor.id;


--
-- Name: comment_closing; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_closing (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    closed_by bigint NOT NULL,
    reason character varying NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.comment_closing OWNER TO snowdrift_test;

--
-- Name: comment_closing_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_closing_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_closing_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_closing_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_closing_id_seq OWNED BY comment_closing.id;


--
-- Name: comment_flagging; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_flagging (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    flagger bigint NOT NULL,
    comment bigint NOT NULL,
    message character varying
);


ALTER TABLE public.comment_flagging OWNER TO snowdrift_test;

--
-- Name: comment_flagging_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_flagging_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_flagging_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_flagging_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_flagging_id_seq OWNED BY comment_flagging.id;


--
-- Name: comment_flagging_reason; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_flagging_reason (
    id integer NOT NULL,
    flagging bigint NOT NULL,
    reason character varying NOT NULL
);


ALTER TABLE public.comment_flagging_reason OWNER TO snowdrift_test;

--
-- Name: comment_flagging_reason_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_flagging_reason_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_flagging_reason_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_flagging_reason_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_flagging_reason_id_seq OWNED BY comment_flagging_reason.id;


--
-- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_id_seq OWNED BY comment.id;


--
-- Name: comment_rethread; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_rethread (
    id integer NOT NULL,
    rethread bigint NOT NULL,
    old_comment bigint NOT NULL,
    new_comment bigint NOT NULL
);


ALTER TABLE public.comment_rethread OWNER TO snowdrift_test;

--
-- Name: comment_rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_rethread_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_rethread_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_rethread_id_seq OWNED BY comment_rethread.id;


--
-- Name: comment_retracting; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_retracting (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    reason character varying NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.comment_retracting OWNER TO snowdrift_test;

--
-- Name: comment_retracting_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_retracting_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_retracting_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_retracting_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_retracting_id_seq OWNED BY comment_retracting.id;


--
-- Name: comment_retraction; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_retraction (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    reason character varying NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.comment_retraction OWNER TO snowdrift_test;

--
-- Name: comment_retraction_id_seq1; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_retraction_id_seq1
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_retraction_id_seq1 OWNER TO snowdrift_test;

--
-- Name: comment_retraction_id_seq1; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_retraction_id_seq1 OWNED BY comment_retraction.id;


--
-- Name: comment_tag; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_tag (
    id integer NOT NULL,
    comment bigint NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    count bigint DEFAULT 1 NOT NULL
);


ALTER TABLE public.comment_tag OWNER TO snowdrift_test;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_tag_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_tag_id_seq OWNED BY comment_tag.id;


--
-- Name: committee_user; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE committee_user (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL
);


ALTER TABLE public.committee_user OWNER TO snowdrift_test;

--
-- Name: committee_user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE committee_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.committee_user_id_seq OWNER TO snowdrift_test;

--
-- Name: committee_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE committee_user_id_seq OWNED BY committee_user.id;


--
-- Name: database_version; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE database_version (
    id integer NOT NULL,
    last_migration bigint NOT NULL
);


ALTER TABLE public.database_version OWNER TO snowdrift_test;

--
-- Name: database_version_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE database_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.database_version_id_seq OWNER TO snowdrift_test;

--
-- Name: database_version_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE database_version_id_seq OWNED BY database_version.id;


--
-- Name: default_tag_color; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE default_tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.default_tag_color OWNER TO snowdrift_test;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE default_tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.default_tag_color_id_seq OWNER TO snowdrift_test;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE default_tag_color_id_seq OWNED BY default_tag_color.id;


--
-- Name: delete_confirmation; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE delete_confirmation (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    email character varying NOT NULL,
    uri character varying NOT NULL,
    sent boolean DEFAULT false NOT NULL
);


ALTER TABLE public.delete_confirmation OWNER TO snowdrift_test;

--
-- Name: delete_confirmation_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE delete_confirmation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.delete_confirmation_id_seq OWNER TO snowdrift_test;

--
-- Name: delete_confirmation_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE delete_confirmation_id_seq OWNED BY delete_confirmation.id;


--
-- Name: deprecated_tag; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE deprecated_tag (
    id integer NOT NULL,
    project bigint NOT NULL,
    tag bigint NOT NULL,
    reason character varying NOT NULL
);


ALTER TABLE public.deprecated_tag OWNER TO snowdrift_test;

--
-- Name: deprecated_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE deprecated_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.deprecated_tag_id_seq OWNER TO snowdrift_test;

--
-- Name: deprecated_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE deprecated_tag_id_seq OWNED BY deprecated_tag.id;


--
-- Name: discussion; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE discussion (
    id integer NOT NULL,
    nothing bigint NOT NULL
);


ALTER TABLE public.discussion OWNER TO snowdrift_test;

--
-- Name: discussion_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE discussion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.discussion_id_seq OWNER TO snowdrift_test;

--
-- Name: discussion_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE discussion_id_seq OWNED BY discussion.id;


--
-- Name: doc; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE doc (
    id integer NOT NULL,
    name character varying NOT NULL,
    current_version bigint NOT NULL
);


ALTER TABLE public.doc OWNER TO snowdrift_test;

--
-- Name: doc_event; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE doc_event (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    doc bigint NOT NULL,
    blessed_version bigint NOT NULL
);


ALTER TABLE public.doc_event OWNER TO snowdrift_test;

--
-- Name: doc_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE doc_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_event_id_seq OWNER TO snowdrift_test;

--
-- Name: doc_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE doc_event_id_seq OWNED BY doc_event.id;


--
-- Name: doc_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE doc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_id_seq OWNER TO snowdrift_test;

--
-- Name: doc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE doc_id_seq OWNED BY doc.id;


--
-- Name: email_verification; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE email_verification (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    sent boolean DEFAULT false NOT NULL,
    email character varying NOT NULL,
    uri character varying NOT NULL
);


ALTER TABLE public.email_verification OWNER TO snowdrift_test;

--
-- Name: email_verification_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE email_verification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.email_verification_id_seq OWNER TO snowdrift_test;

--
-- Name: email_verification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE email_verification_id_seq OWNED BY email_verification.id;


--
-- Name: event_blog_post; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_blog_post (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    post bigint NOT NULL
);


ALTER TABLE public.event_blog_post OWNER TO snowdrift_test;

--
-- Name: event_blog_post_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_blog_post_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_blog_post_id_seq OWNER TO snowdrift_test;

--
-- Name: event_blog_post_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_blog_post_id_seq OWNED BY event_blog_post.id;


--
-- Name: event_comment_closing; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_comment_closing (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    comment_closing bigint NOT NULL
);


ALTER TABLE public.event_comment_closing OWNER TO snowdrift_test;

--
-- Name: event_comment_closing_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_comment_closing_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_comment_closing_id_seq OWNER TO snowdrift_test;

--
-- Name: event_comment_closing_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_comment_closing_id_seq OWNED BY event_comment_closing.id;


--
-- Name: event_comment_pending; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_comment_pending (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ts timestamp with time zone NOT NULL
);


ALTER TABLE public.event_comment_pending OWNER TO snowdrift_test;

--
-- Name: event_comment_pending_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_comment_pending_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_comment_pending_id_seq OWNER TO snowdrift_test;

--
-- Name: event_comment_pending_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_comment_pending_id_seq OWNED BY event_comment_pending.id;


--
-- Name: event_comment_posted; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_comment_posted (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ts timestamp with time zone NOT NULL
);


ALTER TABLE public.event_comment_posted OWNER TO snowdrift_test;

--
-- Name: event_comment_posted_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_comment_posted_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_comment_posted_id_seq OWNER TO snowdrift_test;

--
-- Name: event_comment_posted_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_comment_posted_id_seq OWNED BY event_comment_posted.id;


--
-- Name: event_comment_rethreaded; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_comment_rethreaded (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    rethread bigint NOT NULL
);


ALTER TABLE public.event_comment_rethreaded OWNER TO snowdrift_test;

--
-- Name: event_comment_rethreaded_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_comment_rethreaded_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_comment_rethreaded_id_seq OWNER TO snowdrift_test;

--
-- Name: event_comment_rethreaded_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_comment_rethreaded_id_seq OWNED BY event_comment_rethreaded.id;


--
-- Name: event_deleted_pledge; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_deleted_pledge (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL
);


ALTER TABLE public.event_deleted_pledge OWNER TO snowdrift_test;

--
-- Name: event_deleted_pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_deleted_pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_deleted_pledge_id_seq OWNER TO snowdrift_test;

--
-- Name: event_deleted_pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_deleted_pledge_id_seq OWNED BY event_deleted_pledge.id;


--
-- Name: event_new_pledge; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_new_pledge (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    shares_pledged bigint NOT NULL
);


ALTER TABLE public.event_new_pledge OWNER TO snowdrift_test;

--
-- Name: event_new_pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_new_pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_new_pledge_id_seq OWNER TO snowdrift_test;

--
-- Name: event_new_pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_new_pledge_id_seq OWNED BY event_new_pledge.id;


--
-- Name: event_project_notification_sent; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_project_notification_sent (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    notification bigint NOT NULL
);


ALTER TABLE public.event_project_notification_sent OWNER TO snowdrift_test;

--
-- Name: event_project_notification_sent_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_project_notification_sent_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_project_notification_sent_id_seq OWNER TO snowdrift_test;

--
-- Name: event_project_notification_sent_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_project_notification_sent_id_seq OWNED BY event_project_notification_sent.id;


--
-- Name: event_ticket_claimed; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_ticket_claimed (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    claim bigint,
    old_claim bigint
);


ALTER TABLE public.event_ticket_claimed OWNER TO snowdrift_test;

--
-- Name: event_ticket_claimed_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_ticket_claimed_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_ticket_claimed_id_seq OWNER TO snowdrift_test;

--
-- Name: event_ticket_claimed_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_ticket_claimed_id_seq OWNED BY event_ticket_claimed.id;


--
-- Name: event_ticket_unclaimed; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_ticket_unclaimed (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    claim bigint NOT NULL
);


ALTER TABLE public.event_ticket_unclaimed OWNER TO snowdrift_test;

--
-- Name: event_ticket_unclaimed_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_ticket_unclaimed_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_ticket_unclaimed_id_seq OWNER TO snowdrift_test;

--
-- Name: event_ticket_unclaimed_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_ticket_unclaimed_id_seq OWNED BY event_ticket_unclaimed.id;


--
-- Name: event_updated_pledge; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_updated_pledge (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    old_shares bigint NOT NULL,
    shares_pledged bigint NOT NULL
);


ALTER TABLE public.event_updated_pledge OWNER TO snowdrift_test;

--
-- Name: event_updated_pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_updated_pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_updated_pledge_id_seq OWNER TO snowdrift_test;

--
-- Name: event_updated_pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_updated_pledge_id_seq OWNED BY event_updated_pledge.id;


--
-- Name: event_user_notification_sent; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_user_notification_sent (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    notification bigint NOT NULL
);


ALTER TABLE public.event_user_notification_sent OWNER TO snowdrift_test;

--
-- Name: event_user_notification_sent_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_user_notification_sent_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_user_notification_sent_id_seq OWNER TO snowdrift_test;

--
-- Name: event_user_notification_sent_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_user_notification_sent_id_seq OWNED BY event_user_notification_sent.id;


--
-- Name: event_wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_wiki_edit (
    id integer NOT NULL,
    wiki_edit bigint NOT NULL,
    ts timestamp with time zone NOT NULL
);


ALTER TABLE public.event_wiki_edit OWNER TO snowdrift_test;

--
-- Name: event_wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_wiki_edit_id_seq OWNER TO snowdrift_test;

--
-- Name: event_wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_wiki_edit_id_seq OWNED BY event_wiki_edit.id;


--
-- Name: event_wiki_page; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE event_wiki_page (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    wiki_page bigint NOT NULL
);


ALTER TABLE public.event_wiki_page OWNER TO snowdrift_test;

--
-- Name: event_wiki_page_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE event_wiki_page_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.event_wiki_page_id_seq OWNER TO snowdrift_test;

--
-- Name: event_wiki_page_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE event_wiki_page_id_seq OWNED BY event_wiki_page.id;


--
-- Name: image; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE image (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    uploader bigint NOT NULL,
    project bigint,
    name character varying NOT NULL,
    origin character varying,
    format bytea NOT NULL,
    data bytea NOT NULL
);


ALTER TABLE public.image OWNER TO snowdrift_test;

--
-- Name: image_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE image_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.image_id_seq OWNER TO snowdrift_test;

--
-- Name: image_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE image_id_seq OWNED BY image.id;


--
-- Name: interest; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE interest (
    id integer NOT NULL,
    description character varying NOT NULL
);


ALTER TABLE public.interest OWNER TO snowdrift_test;

--
-- Name: interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.interest_id_seq OWNER TO snowdrift_test;

--
-- Name: interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE interest_id_seq OWNED BY interest.id;


--
-- Name: invite; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE invite (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    project bigint NOT NULL,
    code character varying NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL,
    tag character varying NOT NULL,
    redeemed boolean NOT NULL,
    redeemed_ts timestamp with time zone,
    redeemed_by bigint
);


ALTER TABLE public.invite OWNER TO snowdrift_test;

--
-- Name: invite_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE invite_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.invite_id_seq OWNER TO snowdrift_test;

--
-- Name: invite_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE invite_id_seq OWNED BY invite.id;


--
-- Name: license; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE license (
    id integer NOT NULL,
    name character varying NOT NULL,
    type character varying NOT NULL,
    project_types character varying NOT NULL,
    text character varying NOT NULL,
    website character varying NOT NULL
);


ALTER TABLE public.license OWNER TO snowdrift_test;

--
-- Name: license_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE license_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.license_id_seq OWNER TO snowdrift_test;

--
-- Name: license_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE license_id_seq OWNED BY license.id;


--
-- Name: manual_establishment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE manual_establishment (
    id integer NOT NULL,
    established_user bigint NOT NULL,
    establishing_user bigint NOT NULL
);


ALTER TABLE public.manual_establishment OWNER TO snowdrift_test;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE manual_establishment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.manual_establishment_id_seq OWNER TO snowdrift_test;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE manual_establishment_id_seq OWNED BY manual_establishment.id;


--
-- Name: payday; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE payday (
    id integer NOT NULL,
    date timestamp with time zone NOT NULL
);


ALTER TABLE public.payday OWNER TO snowdrift_test;

--
-- Name: payday_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE payday_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.payday_id_seq OWNER TO snowdrift_test;

--
-- Name: payday_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE payday_id_seq OWNED BY payday.id;


--
-- Name: pledge; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE pledge (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL,
    funded_shares bigint NOT NULL,
    created_ts timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.pledge OWNER TO snowdrift_test;

--
-- Name: pledge_form_rendered; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE pledge_form_rendered (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    "order" character varying NOT NULL,
    project bigint NOT NULL,
    "user" bigint
);


ALTER TABLE public.pledge_form_rendered OWNER TO snowdrift_test;

--
-- Name: pledge_form_rendered_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE pledge_form_rendered_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pledge_form_rendered_id_seq OWNER TO snowdrift_test;

--
-- Name: pledge_form_rendered_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE pledge_form_rendered_id_seq OWNED BY pledge_form_rendered.id;


--
-- Name: pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pledge_id_seq OWNER TO snowdrift_test;

--
-- Name: pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE pledge_id_seq OWNED BY pledge.id;


--
-- Name: project; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    name character varying NOT NULL,
    handle character varying NOT NULL,
    description character varying NOT NULL,
    account bigint NOT NULL,
    share_value bigint NOT NULL,
    last_payday bigint,
    github_repo character varying,
    discussion bigint DEFAULT nextval('discussion_id_seq'::regclass) NOT NULL,
    public boolean DEFAULT true NOT NULL,
    blurb character varying NOT NULL,
    logo character varying
);


ALTER TABLE public.project OWNER TO snowdrift_test;

--
-- Name: project_blog_comment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_blog_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    blog bigint NOT NULL
);


ALTER TABLE public.project_blog_comment OWNER TO snowdrift_test;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_blog_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_comment_id_seq OWNER TO snowdrift_test;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_blog_comment_id_seq OWNED BY project_blog_comment.id;


--
-- Name: project_blog_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_blog_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_id_seq OWNER TO snowdrift_test;

--
-- Name: project_blog_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_blog_id_seq OWNED BY blog_post.id;


--
-- Name: project_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_id_seq OWNER TO snowdrift_test;

--
-- Name: project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_id_seq OWNED BY project.id;


--
-- Name: project_last_update; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_last_update (
    id integer NOT NULL,
    project bigint NOT NULL,
    update bigint NOT NULL
);


ALTER TABLE public.project_last_update OWNER TO snowdrift_test;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_last_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_last_update_id_seq OWNER TO snowdrift_test;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_last_update_id_seq OWNED BY project_last_update.id;


--
-- Name: project_notification; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_notification (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    type character varying NOT NULL,
    "to" bigint NOT NULL,
    project bigint NOT NULL,
    content character varying NOT NULL,
    archived boolean NOT NULL
);


ALTER TABLE public.project_notification OWNER TO snowdrift_test;

--
-- Name: project_notification_email; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_notification_email (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    type character varying NOT NULL,
    "to" bigint NOT NULL,
    project bigint NOT NULL,
    content character varying NOT NULL
);


ALTER TABLE public.project_notification_email OWNER TO snowdrift_test;

--
-- Name: project_notification_email_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_notification_email_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_notification_email_id_seq OWNER TO snowdrift_test;

--
-- Name: project_notification_email_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_notification_email_id_seq OWNED BY project_notification_email.id;


--
-- Name: project_notification_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_notification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_notification_id_seq OWNER TO snowdrift_test;

--
-- Name: project_notification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_notification_id_seq OWNED BY project_notification.id;


--
-- Name: project_notification_pref; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_notification_pref (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    type character varying NOT NULL,
    delivery character varying NOT NULL
);


ALTER TABLE public.project_notification_pref OWNER TO snowdrift_test;

--
-- Name: project_notification_pref_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_notification_pref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_notification_pref_id_seq OWNER TO snowdrift_test;

--
-- Name: project_notification_pref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_notification_pref_id_seq OWNED BY project_notification_pref.id;


--
-- Name: project_signup; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_signup (
    id integer NOT NULL,
    name character varying NOT NULL,
    website character varying,
    handle character varying NOT NULL,
    start_date character varying NOT NULL,
    licenses character varying NOT NULL,
    licenses_comment character varying,
    categories character varying NOT NULL,
    categories_comment character varying,
    location character varying,
    legal_status character varying NOT NULL,
    legal_status_comment character varying,
    coop_status character varying NOT NULL,
    applicant_role character varying NOT NULL,
    mission character varying NOT NULL,
    goals character varying NOT NULL,
    funds_use character varying NOT NULL,
    additional_info character varying
);


ALTER TABLE public.project_signup OWNER TO snowdrift_test;

--
-- Name: project_signup_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_signup_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_signup_id_seq OWNER TO snowdrift_test;

--
-- Name: project_signup_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_signup_id_seq OWNED BY project_signup.id;


--
-- Name: project_tag; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_tag (
    id integer NOT NULL,
    project bigint NOT NULL,
    tag bigint NOT NULL
);


ALTER TABLE public.project_tag OWNER TO snowdrift_test;

--
-- Name: project_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_tag_id_seq OWNER TO snowdrift_test;

--
-- Name: project_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_tag_id_seq OWNED BY project_tag.id;


--
-- Name: project_update; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_update (
    id integer NOT NULL,
    updated_ts timestamp with time zone NOT NULL,
    project bigint NOT NULL,
    author bigint NOT NULL,
    description character varying NOT NULL,
    blurb character varying NOT NULL
);


ALTER TABLE public.project_update OWNER TO snowdrift_test;

--
-- Name: project_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_update_id_seq OWNER TO snowdrift_test;

--
-- Name: project_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_update_id_seq OWNED BY project_update.id;


--
-- Name: project_user_role; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_user_role (
    id integer NOT NULL,
    project bigint NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL
);


ALTER TABLE public.project_user_role OWNER TO snowdrift_test;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_user_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_user_role_id_seq OWNER TO snowdrift_test;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_user_role_id_seq OWNED BY project_user_role.id;


--
-- Name: reset_password; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE reset_password (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    email character varying NOT NULL,
    uri character varying NOT NULL,
    sent boolean DEFAULT false NOT NULL
);


ALTER TABLE public.reset_password OWNER TO snowdrift_test;

--
-- Name: reset_password_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE reset_password_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.reset_password_id_seq OWNER TO snowdrift_test;

--
-- Name: reset_password_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE reset_password_id_seq OWNED BY reset_password.id;


--
-- Name: rethread; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE rethread (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    moderator bigint NOT NULL,
    old_comment bigint NOT NULL,
    reason character varying NOT NULL,
    new_comment bigint NOT NULL
);


ALTER TABLE public.rethread OWNER TO snowdrift_test;

--
-- Name: rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE rethread_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.rethread_id_seq OWNER TO snowdrift_test;

--
-- Name: rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE rethread_id_seq OWNED BY rethread.id;


--
-- Name: role_event; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE role_event (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL,
    project bigint NOT NULL,
    added boolean NOT NULL
);


ALTER TABLE public.role_event OWNER TO snowdrift_test;

--
-- Name: role_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE role_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.role_event_id_seq OWNER TO snowdrift_test;

--
-- Name: role_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE role_event_id_seq OWNED BY role_event.id;


--
-- Name: shares_pledged; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE shares_pledged (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL,
    render bigint NOT NULL
);


ALTER TABLE public.shares_pledged OWNER TO snowdrift_test;

--
-- Name: shares_pledged_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE shares_pledged_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.shares_pledged_id_seq OWNER TO snowdrift_test;

--
-- Name: shares_pledged_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE shares_pledged_id_seq OWNED BY shares_pledged.id;


--
-- Name: tag; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE tag (
    id integer NOT NULL,
    name character varying NOT NULL
);


ALTER TABLE public.tag OWNER TO snowdrift_test;

--
-- Name: tag_color; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.tag_color OWNER TO snowdrift_test;

--
-- Name: tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_color_id_seq OWNER TO snowdrift_test;

--
-- Name: tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE tag_color_id_seq OWNED BY tag_color.id;


--
-- Name: tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_id_seq OWNER TO snowdrift_test;

--
-- Name: tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE tag_id_seq OWNED BY tag.id;


--
-- Name: ticket; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE ticket (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    name character varying NOT NULL,
    comment bigint NOT NULL,
    updated_ts timestamp with time zone NOT NULL
);


ALTER TABLE public.ticket OWNER TO snowdrift_test;

--
-- Name: ticket_claiming; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE ticket_claiming (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    "user" bigint NOT NULL,
    ticket bigint NOT NULL,
    note character varying
);


ALTER TABLE public.ticket_claiming OWNER TO snowdrift_test;

--
-- Name: ticket_claiming_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE ticket_claiming_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ticket_claiming_id_seq OWNER TO snowdrift_test;

--
-- Name: ticket_claiming_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE ticket_claiming_id_seq OWNED BY ticket_claiming.id;


--
-- Name: ticket_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE ticket_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ticket_id_seq OWNER TO snowdrift_test;

--
-- Name: ticket_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE ticket_id_seq OWNED BY ticket.id;


--
-- Name: ticket_old_claiming; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE ticket_old_claiming (
    id integer NOT NULL,
    claim_ts timestamp with time zone NOT NULL,
    "user" bigint NOT NULL,
    ticket bigint NOT NULL,
    note character varying,
    release_note character varying,
    released_ts timestamp with time zone NOT NULL
);


ALTER TABLE public.ticket_old_claiming OWNER TO snowdrift_test;

--
-- Name: ticket_old_claiming_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE ticket_old_claiming_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ticket_old_claiming_id_seq OWNER TO snowdrift_test;

--
-- Name: ticket_old_claiming_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE ticket_old_claiming_id_seq OWNED BY ticket_old_claiming.id;


--
-- Name: transaction; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE transaction (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    credit bigint,
    debit bigint,
    amount bigint NOT NULL,
    reason character varying NOT NULL,
    info character varying,
    payday bigint
);


ALTER TABLE public.transaction OWNER TO snowdrift_test;

--
-- Name: transaction_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.transaction_id_seq OWNER TO snowdrift_test;

--
-- Name: transaction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE transaction_id_seq OWNED BY transaction.id;


--
-- Name: unapproved_comment_notification; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE unapproved_comment_notification (
    id integer NOT NULL,
    comment bigint NOT NULL,
    notification bigint NOT NULL
);


ALTER TABLE public.unapproved_comment_notification OWNER TO snowdrift_test;

--
-- Name: unapproved_comment_notification_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE unapproved_comment_notification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.unapproved_comment_notification_id_seq OWNER TO snowdrift_test;

--
-- Name: unapproved_comment_notification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE unapproved_comment_notification_id_seq OWNED BY unapproved_comment_notification.id;


--
-- Name: unnamed_image; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE unnamed_image (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    uploader bigint NOT NULL,
    project bigint,
    name character varying,
    origin character varying,
    format bytea NOT NULL,
    data bytea NOT NULL
);


ALTER TABLE public.unnamed_image OWNER TO snowdrift_test;

--
-- Name: unnamed_image_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE unnamed_image_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.unnamed_image_id_seq OWNER TO snowdrift_test;

--
-- Name: unnamed_image_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE unnamed_image_id_seq OWNED BY unnamed_image.id;


--
-- Name: user; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE "user" (
    id integer NOT NULL,
    ident character varying NOT NULL,
    hash character varying,
    salt character varying,
    name character varying,
    account bigint NOT NULL,
    avatar character varying,
    blurb character varying,
    statement character varying,
    irc_nick character varying,
    read_notifications timestamp with time zone DEFAULT now() NOT NULL,
    read_applications timestamp with time zone DEFAULT now() NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    established character varying DEFAULT 'EstUnestablished'::character varying NOT NULL,
    discussion bigint DEFAULT nextval('discussion_id_seq'::regclass) NOT NULL,
    email character varying,
    languages character varying NOT NULL,
    email_verified boolean DEFAULT false NOT NULL
);


ALTER TABLE public."user" OWNER TO snowdrift_test;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_id_seq OWNER TO snowdrift_test;

--
-- Name: user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_id_seq OWNED BY "user".id;


--
-- Name: user_message_pref; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE user_message_pref (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    type character varying NOT NULL,
    delivery character varying NOT NULL
);


ALTER TABLE public.user_message_pref OWNER TO snowdrift_test;

--
-- Name: user_message_pref_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_message_pref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_message_pref_id_seq OWNER TO snowdrift_test;

--
-- Name: user_message_pref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_message_pref_id_seq OWNED BY user_message_pref.id;


--
-- Name: user_notification; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE user_notification (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    type character varying NOT NULL,
    "to" bigint NOT NULL,
    content character varying NOT NULL,
    archived boolean NOT NULL
);


ALTER TABLE public.user_notification OWNER TO snowdrift_test;

--
-- Name: user_notification_email; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE user_notification_email (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    type character varying NOT NULL,
    "to" bigint NOT NULL,
    content character varying NOT NULL
);


ALTER TABLE public.user_notification_email OWNER TO snowdrift_test;

--
-- Name: user_notification_email_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_notification_email_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_notification_email_id_seq OWNER TO snowdrift_test;

--
-- Name: user_notification_email_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_notification_email_id_seq OWNED BY user_notification_email.id;


--
-- Name: user_notification_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_notification_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_notification_id_seq OWNER TO snowdrift_test;

--
-- Name: user_notification_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_notification_id_seq OWNED BY user_notification.id;


--
-- Name: user_notification_pref; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE user_notification_pref (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    type character varying NOT NULL,
    delivery character varying NOT NULL
);


ALTER TABLE public.user_notification_pref OWNER TO snowdrift_test;

--
-- Name: user_notification_pref_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_notification_pref_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_notification_pref_id_seq OWNER TO snowdrift_test;

--
-- Name: user_notification_pref_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_notification_pref_id_seq OWNED BY user_notification_pref.id;


--
-- Name: user_setting; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE user_setting (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    setting character varying NOT NULL,
    value character varying NOT NULL
);


ALTER TABLE public.user_setting OWNER TO snowdrift_test;

--
-- Name: user_setting_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_setting_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_setting_id_seq OWNER TO snowdrift_test;

--
-- Name: user_setting_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_setting_id_seq OWNED BY user_setting.id;


--
-- Name: user_watching_project; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE user_watching_project (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL
);


ALTER TABLE public.user_watching_project OWNER TO snowdrift_test;

--
-- Name: user_watching_project_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_watching_project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_watching_project_id_seq OWNER TO snowdrift_test;

--
-- Name: user_watching_project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_watching_project_id_seq OWNED BY user_watching_project.id;


--
-- Name: view_comment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE view_comment (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.view_comment OWNER TO snowdrift_test;

--
-- Name: view_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE view_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.view_comment_id_seq OWNER TO snowdrift_test;

--
-- Name: view_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE view_comment_id_seq OWNED BY view_comment.id;


--
-- Name: view_time; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE view_time (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    type character varying NOT NULL,
    "time" timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.view_time OWNER TO snowdrift_test;

--
-- Name: view_time_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE view_time_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.view_time_id_seq OWNER TO snowdrift_test;

--
-- Name: view_time_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE view_time_id_seq OWNED BY view_time.id;


--
-- Name: view_wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE view_wiki_edit (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    edit bigint NOT NULL
);


ALTER TABLE public.view_wiki_edit OWNER TO snowdrift_test;

--
-- Name: view_wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE view_wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.view_wiki_edit_id_seq OWNER TO snowdrift_test;

--
-- Name: view_wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE view_wiki_edit_id_seq OWNED BY view_wiki_edit.id;


--
-- Name: volunteer_application; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE volunteer_application (
    id integer NOT NULL,
    created_ts timestamp with time zone NOT NULL,
    project bigint NOT NULL,
    "user" bigint NOT NULL,
    name character varying NOT NULL,
    email character varying NOT NULL,
    other_contact_info character varying,
    website character varying,
    location character varying,
    experience character varying,
    comments character varying
);


ALTER TABLE public.volunteer_application OWNER TO snowdrift_test;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE volunteer_application_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_application_id_seq OWNER TO snowdrift_test;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE volunteer_application_id_seq OWNED BY volunteer_application.id;


--
-- Name: volunteer_interest; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE volunteer_interest (
    id integer NOT NULL,
    volunteer bigint NOT NULL,
    interest bigint NOT NULL
);


ALTER TABLE public.volunteer_interest OWNER TO snowdrift_test;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE volunteer_interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_interest_id_seq OWNER TO snowdrift_test;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE volunteer_interest_id_seq OWNED BY volunteer_interest.id;


--
-- Name: watched_subthread; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE watched_subthread (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    "user" bigint NOT NULL,
    root bigint NOT NULL
);


ALTER TABLE public.watched_subthread OWNER TO snowdrift_test;

--
-- Name: watched_subthread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE watched_subthread_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.watched_subthread_id_seq OWNER TO snowdrift_test;

--
-- Name: watched_subthread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE watched_subthread_id_seq OWNED BY watched_subthread.id;


--
-- Name: wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_edit (
    id integer NOT NULL,
    ts timestamp with time zone NOT NULL,
    "user" bigint NOT NULL,
    page bigint NOT NULL,
    content character varying NOT NULL,
    comment character varying,
    language character varying NOT NULL
);


ALTER TABLE public.wiki_edit OWNER TO snowdrift_test;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_edit_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_edit_id_seq OWNED BY wiki_edit.id;


--
-- Name: wiki_last_edit; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_last_edit (
    id integer NOT NULL,
    page bigint NOT NULL,
    edit bigint NOT NULL,
    language character varying NOT NULL
);


ALTER TABLE public.wiki_last_edit OWNER TO snowdrift_test;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_last_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_last_edit_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_last_edit_id_seq OWNED BY wiki_last_edit.id;


--
-- Name: wiki_page; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_page (
    id integer NOT NULL,
    project bigint NOT NULL,
    permission_level character varying NOT NULL,
    discussion bigint NOT NULL,
    created_ts timestamp with time zone DEFAULT now() NOT NULL,
    "user" bigint NOT NULL
);


ALTER TABLE public.wiki_page OWNER TO snowdrift_test;

--
-- Name: wiki_page_comment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_page_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    page bigint NOT NULL
);


ALTER TABLE public.wiki_page_comment OWNER TO snowdrift_test;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_page_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_comment_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_page_comment_id_seq OWNED BY wiki_page_comment.id;


--
-- Name: wiki_page_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_page_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_page_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_page_id_seq OWNED BY wiki_page.id;


--
-- Name: wiki_target; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_target (
    id integer NOT NULL,
    page bigint NOT NULL,
    project bigint NOT NULL,
    target character varying NOT NULL,
    language character varying NOT NULL
);


ALTER TABLE public.wiki_target OWNER TO snowdrift_test;

--
-- Name: wiki_target_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_target_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_target_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_target_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_target_id_seq OWNED BY wiki_target.id;


--
-- Name: wiki_translation; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_translation (
    id integer NOT NULL,
    edit bigint NOT NULL,
    source bigint NOT NULL,
    complete boolean NOT NULL
);


ALTER TABLE public.wiki_translation OWNER TO snowdrift_test;

--
-- Name: wiki_translation_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_translation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_translation_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_translation_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_translation_id_seq OWNED BY wiki_translation.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY a ALTER COLUMN id SET DEFAULT nextval('a_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY account ALTER COLUMN id SET DEFAULT nextval('account_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY blog_post ALTER COLUMN id SET DEFAULT nextval('project_blog_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY build ALTER COLUMN id SET DEFAULT nextval('build_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment ALTER COLUMN id SET DEFAULT nextval('comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_ancestor ALTER COLUMN id SET DEFAULT nextval('comment_ancestor_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_closing ALTER COLUMN id SET DEFAULT nextval('comment_closing_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_flagging ALTER COLUMN id SET DEFAULT nextval('comment_flagging_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_flagging_reason ALTER COLUMN id SET DEFAULT nextval('comment_flagging_reason_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_rethread_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_retracting ALTER COLUMN id SET DEFAULT nextval('comment_retracting_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq1'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag ALTER COLUMN id SET DEFAULT nextval('comment_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY committee_user ALTER COLUMN id SET DEFAULT nextval('committee_user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY database_version ALTER COLUMN id SET DEFAULT nextval('database_version_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY default_tag_color ALTER COLUMN id SET DEFAULT nextval('default_tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY delete_confirmation ALTER COLUMN id SET DEFAULT nextval('delete_confirmation_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY deprecated_tag ALTER COLUMN id SET DEFAULT nextval('deprecated_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY discussion ALTER COLUMN id SET DEFAULT nextval('discussion_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc ALTER COLUMN id SET DEFAULT nextval('doc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc_event ALTER COLUMN id SET DEFAULT nextval('doc_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY email_verification ALTER COLUMN id SET DEFAULT nextval('email_verification_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_blog_post ALTER COLUMN id SET DEFAULT nextval('event_blog_post_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_closing ALTER COLUMN id SET DEFAULT nextval('event_comment_closing_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_pending ALTER COLUMN id SET DEFAULT nextval('event_comment_pending_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_posted ALTER COLUMN id SET DEFAULT nextval('event_comment_posted_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_rethreaded ALTER COLUMN id SET DEFAULT nextval('event_comment_rethreaded_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_deleted_pledge ALTER COLUMN id SET DEFAULT nextval('event_deleted_pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_new_pledge ALTER COLUMN id SET DEFAULT nextval('event_new_pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_project_notification_sent ALTER COLUMN id SET DEFAULT nextval('event_project_notification_sent_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_ticket_claimed ALTER COLUMN id SET DEFAULT nextval('event_ticket_claimed_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_ticket_unclaimed ALTER COLUMN id SET DEFAULT nextval('event_ticket_unclaimed_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_updated_pledge ALTER COLUMN id SET DEFAULT nextval('event_updated_pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_user_notification_sent ALTER COLUMN id SET DEFAULT nextval('event_user_notification_sent_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_wiki_edit ALTER COLUMN id SET DEFAULT nextval('event_wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_wiki_page ALTER COLUMN id SET DEFAULT nextval('event_wiki_page_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY image ALTER COLUMN id SET DEFAULT nextval('image_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY interest ALTER COLUMN id SET DEFAULT nextval('interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite ALTER COLUMN id SET DEFAULT nextval('invite_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY license ALTER COLUMN id SET DEFAULT nextval('license_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY manual_establishment ALTER COLUMN id SET DEFAULT nextval('manual_establishment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY payday ALTER COLUMN id SET DEFAULT nextval('payday_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge ALTER COLUMN id SET DEFAULT nextval('pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge_form_rendered ALTER COLUMN id SET DEFAULT nextval('pledge_form_rendered_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project ALTER COLUMN id SET DEFAULT nextval('project_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog_comment ALTER COLUMN id SET DEFAULT nextval('project_blog_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_last_update ALTER COLUMN id SET DEFAULT nextval('project_last_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification ALTER COLUMN id SET DEFAULT nextval('project_notification_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification_email ALTER COLUMN id SET DEFAULT nextval('project_notification_email_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification_pref ALTER COLUMN id SET DEFAULT nextval('project_notification_pref_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_signup ALTER COLUMN id SET DEFAULT nextval('project_signup_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_tag ALTER COLUMN id SET DEFAULT nextval('project_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_update ALTER COLUMN id SET DEFAULT nextval('project_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_user_role ALTER COLUMN id SET DEFAULT nextval('project_user_role_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY reset_password ALTER COLUMN id SET DEFAULT nextval('reset_password_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY rethread ALTER COLUMN id SET DEFAULT nextval('rethread_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY role_event ALTER COLUMN id SET DEFAULT nextval('role_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY shares_pledged ALTER COLUMN id SET DEFAULT nextval('shares_pledged_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag ALTER COLUMN id SET DEFAULT nextval('tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag_color ALTER COLUMN id SET DEFAULT nextval('tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket ALTER COLUMN id SET DEFAULT nextval('ticket_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket_claiming ALTER COLUMN id SET DEFAULT nextval('ticket_claiming_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket_old_claiming ALTER COLUMN id SET DEFAULT nextval('ticket_old_claiming_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction ALTER COLUMN id SET DEFAULT nextval('transaction_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY unapproved_comment_notification ALTER COLUMN id SET DEFAULT nextval('unapproved_comment_notification_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY unnamed_image ALTER COLUMN id SET DEFAULT nextval('unnamed_image_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY "user" ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_message_pref ALTER COLUMN id SET DEFAULT nextval('user_message_pref_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_notification ALTER COLUMN id SET DEFAULT nextval('user_notification_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_notification_email ALTER COLUMN id SET DEFAULT nextval('user_notification_email_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_notification_pref ALTER COLUMN id SET DEFAULT nextval('user_notification_pref_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_setting ALTER COLUMN id SET DEFAULT nextval('user_setting_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_watching_project ALTER COLUMN id SET DEFAULT nextval('user_watching_project_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_comment ALTER COLUMN id SET DEFAULT nextval('view_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_time ALTER COLUMN id SET DEFAULT nextval('view_time_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_wiki_edit ALTER COLUMN id SET DEFAULT nextval('view_wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_application ALTER COLUMN id SET DEFAULT nextval('volunteer_application_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_interest ALTER COLUMN id SET DEFAULT nextval('volunteer_interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY watched_subthread ALTER COLUMN id SET DEFAULT nextval('watched_subthread_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_edit ALTER COLUMN id SET DEFAULT nextval('wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_last_edit ALTER COLUMN id SET DEFAULT nextval('wiki_last_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page ALTER COLUMN id SET DEFAULT nextval('wiki_page_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page_comment ALTER COLUMN id SET DEFAULT nextval('wiki_page_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_target ALTER COLUMN id SET DEFAULT nextval('wiki_target_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_translation ALTER COLUMN id SET DEFAULT nextval('wiki_translation_id_seq'::regclass);


--
-- Data for Name: a; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY a (id) FROM stdin;
\.


--
-- Name: a_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('a_id_seq', 1, false);


--
-- Data for Name: account; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY account (id, balance) FROM stdin;
1	0
2	0
3	0
-1	0
-2	0
\.


--
-- Name: account_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('account_id_seq', 3, true);


--
-- Data for Name: blog_post; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY blog_post (id, ts, title, "user", top_content, project, bottom_content, discussion, handle) FROM stdin;
\.


--
-- Data for Name: build; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY build (id, boot_time, base, diff) FROM stdin;
\.


--
-- Name: build_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('build_id_seq', 59, true);


--
-- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment (id, created_ts, approved_ts, approved_by, parent, "user", text, depth, discussion, visibility, language) FROM stdin;
1	2014-01-21 18:11:03.914397+00	2014-01-21 18:12:36.696658+00	1	\N	1	This is a comment.	0	2	VisPublic	en
2	2014-01-21 18:13:00.273315+00	2014-01-21 18:13:10.464805+00	1	1	1	Replies are threaded.	1	2	VisPublic	en
3	2014-01-21 18:13:57.732222+00	\N	\N	\N	1	When a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.	0	2	VisPublic	en
4	2014-01-21 18:15:30.945499+00	2014-01-21 18:15:37.484472+00	1	\N	1	adding a line starting with "ticket:" such as\n\nticket: this is a ticket\n\nmakes the post show up at /t where all the tickets are listed	0	2	VisPublic	en
5	2014-01-21 22:31:51.496246+00	\N	\N	\N	1	Welcome!	0	7	VisPublic	en
\.


--
-- Data for Name: comment_ancestor; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_ancestor (id, comment, ancestor) FROM stdin;
1	2	1
\.


--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_ancestor_id_seq', 1, true);


--
-- Data for Name: comment_closing; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_closing (id, ts, closed_by, reason, comment) FROM stdin;
\.


--
-- Name: comment_closing_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_closing_id_seq', 1, false);


--
-- Data for Name: comment_flagging; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_flagging (id, ts, flagger, comment, message) FROM stdin;
\.


--
-- Name: comment_flagging_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_flagging_id_seq', 1, false);


--
-- Data for Name: comment_flagging_reason; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_flagging_reason (id, flagging, reason) FROM stdin;
\.


--
-- Name: comment_flagging_reason_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_flagging_reason_id_seq', 1, false);


--
-- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_id_seq', 5, true);


--
-- Data for Name: comment_rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_rethread (id, rethread, old_comment, new_comment) FROM stdin;
\.


--
-- Name: comment_rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_rethread_id_seq', 1, false);


--
-- Data for Name: comment_retracting; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_retracting (id, ts, reason, comment) FROM stdin;
\.


--
-- Name: comment_retracting_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_retracting_id_seq', 1, false);


--
-- Data for Name: comment_retraction; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_retraction (id, ts, reason, comment) FROM stdin;
\.


--
-- Name: comment_retraction_id_seq1; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_retraction_id_seq1', 1, false);


--
-- Data for Name: comment_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_tag (id, comment, tag, "user", count) FROM stdin;
\.


--
-- Name: comment_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_tag_id_seq', 1, false);


--
-- Data for Name: committee_user; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY committee_user (id, created_ts, "user", project) FROM stdin;
\.


--
-- Name: committee_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('committee_user_id_seq', 1, false);


--
-- Data for Name: database_version; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY database_version (id, last_migration) FROM stdin;
1	66
\.


--
-- Name: database_version_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('database_version_id_seq', 1, true);


--
-- Data for Name: default_tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY default_tag_color (id, tag, color) FROM stdin;
\.


--
-- Name: default_tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('default_tag_color_id_seq', 1, false);


--
-- Data for Name: delete_confirmation; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY delete_confirmation (id, "user", email, uri, sent) FROM stdin;
\.


--
-- Name: delete_confirmation_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('delete_confirmation_id_seq', 1, false);


--
-- Data for Name: deprecated_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY deprecated_tag (id, project, tag, reason) FROM stdin;
\.


--
-- Name: deprecated_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('deprecated_tag_id_seq', 1, false);


--
-- Data for Name: discussion; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY discussion (id, nothing) FROM stdin;
1	0
2	0
3	0
4	0
5	0
6	0
7	0
-1	0
-2	0
\.


--
-- Name: discussion_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('discussion_id_seq', 7, true);


--
-- Data for Name: doc; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY doc (id, name, current_version) FROM stdin;
\.


--
-- Data for Name: doc_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY doc_event (id, ts, doc, blessed_version) FROM stdin;
\.


--
-- Name: doc_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('doc_event_id_seq', 1, false);


--
-- Name: doc_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('doc_id_seq', 1, false);


--
-- Data for Name: email_verification; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY email_verification (id, "user", sent, email, uri) FROM stdin;
\.


--
-- Name: email_verification_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('email_verification_id_seq', 1, false);


--
-- Data for Name: event_blog_post; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_blog_post (id, ts, post) FROM stdin;
\.


--
-- Name: event_blog_post_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_blog_post_id_seq', 1, false);


--
-- Data for Name: event_comment_closing; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_comment_closing (id, ts, comment_closing) FROM stdin;
\.


--
-- Name: event_comment_closing_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_comment_closing_id_seq', 1, false);


--
-- Data for Name: event_comment_pending; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_comment_pending (id, comment, ts) FROM stdin;
\.


--
-- Name: event_comment_pending_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_comment_pending_id_seq', 1, false);


--
-- Data for Name: event_comment_posted; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_comment_posted (id, comment, ts) FROM stdin;
\.


--
-- Name: event_comment_posted_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_comment_posted_id_seq', 1, false);


--
-- Data for Name: event_comment_rethreaded; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_comment_rethreaded (id, ts, rethread) FROM stdin;
\.


--
-- Name: event_comment_rethreaded_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_comment_rethreaded_id_seq', 1, false);


--
-- Data for Name: event_deleted_pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_deleted_pledge (id, ts, "user", project, shares) FROM stdin;
\.


--
-- Name: event_deleted_pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_deleted_pledge_id_seq', 1, false);


--
-- Data for Name: event_new_pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_new_pledge (id, ts, shares_pledged) FROM stdin;
\.


--
-- Name: event_new_pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_new_pledge_id_seq', 1, false);


--
-- Data for Name: event_project_notification_sent; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_project_notification_sent (id, ts, notification) FROM stdin;
\.


--
-- Name: event_project_notification_sent_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_project_notification_sent_id_seq', 1, false);


--
-- Data for Name: event_ticket_claimed; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_ticket_claimed (id, ts, claim, old_claim) FROM stdin;
\.


--
-- Name: event_ticket_claimed_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_ticket_claimed_id_seq', 1, false);


--
-- Data for Name: event_ticket_unclaimed; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_ticket_unclaimed (id, ts, claim) FROM stdin;
\.


--
-- Name: event_ticket_unclaimed_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_ticket_unclaimed_id_seq', 1, false);


--
-- Data for Name: event_updated_pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_updated_pledge (id, ts, old_shares, shares_pledged) FROM stdin;
\.


--
-- Name: event_updated_pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_updated_pledge_id_seq', 1, false);


--
-- Data for Name: event_user_notification_sent; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_user_notification_sent (id, ts, notification) FROM stdin;
\.


--
-- Name: event_user_notification_sent_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_user_notification_sent_id_seq', 1, false);


--
-- Data for Name: event_wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_wiki_edit (id, wiki_edit, ts) FROM stdin;
\.


--
-- Name: event_wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_wiki_edit_id_seq', 1, false);


--
-- Data for Name: event_wiki_page; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY event_wiki_page (id, ts, wiki_page) FROM stdin;
\.


--
-- Name: event_wiki_page_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('event_wiki_page_id_seq', 1, false);


--
-- Data for Name: image; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY image (id, ts, uploader, project, name, origin, format, data) FROM stdin;
\.


--
-- Name: image_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('image_id_seq', 1, false);


--
-- Data for Name: interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY interest (id, description) FROM stdin;
\.


--
-- Name: interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('interest_id_seq', 1, false);


--
-- Data for Name: invite; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY invite (id, created_ts, project, code, "user", role, tag, redeemed, redeemed_ts, redeemed_by) FROM stdin;
1	2014-01-21 18:12:09.148007+00	1	df0176d67f1a4063	1	Moderator	admin as also moderator	t	2014-01-21 18:12:24.376052+00	1
2	2014-01-24 23:33:43.323505+00	1	e3ed7c9e1500fc54	1	TeamMember	admin as also team member	t	2014-01-24 23:33:53.481901+00	1
\.


--
-- Name: invite_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('invite_id_seq', 2, true);


--
-- Data for Name: license; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY license (id, name, type, project_types, text, website) FROM stdin;
\.


--
-- Name: license_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('license_id_seq', 1, false);


--
-- Data for Name: manual_establishment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY manual_establishment (id, established_user, establishing_user) FROM stdin;
\.


--
-- Name: manual_establishment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('manual_establishment_id_seq', 1, false);


--
-- Data for Name: payday; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY payday (id, date) FROM stdin;
\.


--
-- Name: payday_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('payday_id_seq', 1, false);


--
-- Data for Name: pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY pledge (id, "user", project, shares, funded_shares, created_ts) FROM stdin;
\.


--
-- Data for Name: pledge_form_rendered; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY pledge_form_rendered (id, ts, "order", project, "user") FROM stdin;
\.


--
-- Name: pledge_form_rendered_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('pledge_form_rendered_id_seq', 1, false);


--
-- Name: pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('pledge_id_seq', 1, false);


--
-- Data for Name: project; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project (id, created_ts, name, handle, description, account, share_value, last_payday, github_repo, discussion, public, blurb, logo) FROM stdin;
1	2013-11-23 11:52:54.632763+00	Snowdrift.coop	snowdrift		2	0	\N	\N	7	t	The Snowdrift.coop site is itself one of the projects.	
\.


--
-- Data for Name: project_blog_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_blog_comment (id, comment, blog) FROM stdin;
\.


--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_blog_comment_id_seq', 1, false);


--
-- Name: project_blog_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_blog_id_seq', 1, false);


--
-- Name: project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_id_seq', 1, true);


--
-- Data for Name: project_last_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_last_update (id, project, update) FROM stdin;
1	1	1
\.


--
-- Name: project_last_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_last_update_id_seq', 1, true);


--
-- Data for Name: project_notification; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_notification (id, created_ts, type, "to", project, content, archived) FROM stdin;
\.


--
-- Data for Name: project_notification_email; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_notification_email (id, created_ts, type, "to", project, content) FROM stdin;
\.


--
-- Name: project_notification_email_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_notification_email_id_seq', 1, false);


--
-- Name: project_notification_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_notification_id_seq', 1, false);


--
-- Data for Name: project_notification_pref; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_notification_pref (id, "user", project, type, delivery) FROM stdin;
\.


--
-- Name: project_notification_pref_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_notification_pref_id_seq', 1, false);


--
-- Data for Name: project_signup; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_signup (id, name, website, handle, start_date, licenses, licenses_comment, categories, categories_comment, location, legal_status, legal_status_comment, coop_status, applicant_role, mission, goals, funds_use, additional_info) FROM stdin;
\.


--
-- Name: project_signup_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_signup_id_seq', 1, false);


--
-- Data for Name: project_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_tag (id, project, tag) FROM stdin;
1	1	1
\.


--
-- Name: project_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_tag_id_seq', 1, true);


--
-- Data for Name: project_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_update (id, updated_ts, project, author, description, blurb) FROM stdin;
1	2014-01-24 21:49:51.132962+00	1	1	MarkdownDiff [(F,"Snowdrift Project"),(S,"The Snowdrift.coop site is itself one of the projects.")]	
\.


--
-- Name: project_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_update_id_seq', 1, true);


--
-- Data for Name: project_user_role; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_user_role (id, project, "user", role) FROM stdin;
2	1	1	Admin
3	1	1	Moderator
4	1	1	TeamMember
5	1	2	Moderator
\.


--
-- Name: project_user_role_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_user_role_id_seq', 5, true);


--
-- Data for Name: reset_password; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY reset_password (id, "user", email, uri, sent) FROM stdin;
\.


--
-- Name: reset_password_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('reset_password_id_seq', 1, false);


--
-- Data for Name: rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY rethread (id, ts, moderator, old_comment, reason, new_comment) FROM stdin;
\.


--
-- Name: rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('rethread_id_seq', 1, false);


--
-- Data for Name: role_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY role_event (id, ts, "user", role, project, added) FROM stdin;
1	2014-01-21 10:12:24.376209+00	1	Moderator	1	t
2	2014-01-24 15:33:53.482076+00	1	TeamMember	1	t
3	2014-03-07 19:13:15.080277+00	2	Moderator	1	t
\.


--
-- Name: role_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('role_event_id_seq', 3, true);


--
-- Data for Name: shares_pledged; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY shares_pledged (id, ts, "user", project, shares, render) FROM stdin;
\.


--
-- Name: shares_pledged_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('shares_pledged_id_seq', 1, false);


--
-- Data for Name: tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY tag (id, name) FROM stdin;
1	website
\.


--
-- Data for Name: tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY tag_color (id, tag, "user", color) FROM stdin;
\.


--
-- Name: tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('tag_color_id_seq', 1, false);


--
-- Name: tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('tag_id_seq', 1, true);


--
-- Data for Name: ticket; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY ticket (id, created_ts, name, comment, updated_ts) FROM stdin;
1	2014-01-21 18:15:30.945499+00	this is a ticket	4	2014-01-21 18:15:30.945499+00
\.


--
-- Data for Name: ticket_claiming; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY ticket_claiming (id, ts, "user", ticket, note) FROM stdin;
\.


--
-- Name: ticket_claiming_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('ticket_claiming_id_seq', 1, false);


--
-- Name: ticket_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('ticket_id_seq', 1, true);


--
-- Data for Name: ticket_old_claiming; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY ticket_old_claiming (id, claim_ts, "user", ticket, note, release_note, released_ts) FROM stdin;
\.


--
-- Name: ticket_old_claiming_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('ticket_old_claiming_id_seq', 1, false);


--
-- Data for Name: transaction; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY transaction (id, ts, credit, debit, amount, reason, info, payday) FROM stdin;
\.


--
-- Name: transaction_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('transaction_id_seq', 1, false);


--
-- Data for Name: unapproved_comment_notification; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY unapproved_comment_notification (id, comment, notification) FROM stdin;
\.


--
-- Name: unapproved_comment_notification_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('unapproved_comment_notification_id_seq', 1, false);


--
-- Data for Name: unnamed_image; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY unnamed_image (id, ts, uploader, project, name, origin, format, data) FROM stdin;
\.


--
-- Name: unnamed_image_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('unnamed_image_id_seq', 1, false);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY "user" (id, ident, hash, salt, name, account, avatar, blurb, statement, irc_nick, read_notifications, read_applications, created_ts, established, discussion, email, languages, email_verified) FROM stdin;
2	test	a090d14299acd2b596b64fb5a46d3587ece359d8	_>4icWF[	Test	3	\N	\N	\N	\N	2014-02-24 01:58:57.856901+00	2014-02-24 01:58:57.856901+00	2014-02-24 01:58:57.856901+00	EstEstablished 2014-09-06 10:06:14.520014 2014-09-06 10:06:14.520014 "FOUNDATIONAL"	6	\N	["sen"]	f
1	admin	8bf2d491387febc07e5d8fd15a4140b28473566e	P^YTN3G:	Admin	1	\N	Admin is the name for the test user in our devDB database that comes with the code. Log in as admin with passphrase: admin	\N	\N	2014-01-21 22:58:23.380462+00	2013-11-23 19:31:18.982213+00	2012-11-09 08:00:00+00	EstEstablished 2014-09-06 10:06:14.520014 2014-09-06 10:06:14.520014 "FOUNDATIONAL"	5	\N	["sen"]	f
-1	anonymous	\N	\N	anonymous user	-1	\N	\N	\N	\N	2015-07-14 23:55:35.574688+01	2015-07-14 23:55:35.574688+01	2012-11-09 08:00:00+00	EstEligible 2014-08-29 02:20:29.479083 UTC "Anonymous User"	-1	\N	["sen"]	f
-2	deleted	\N	\N	deleted user	-2	\N	\N	\N	\N	2015-07-14 22:55:35.574688+01	2015-07-14 22:55:35.574688+01	2012-11-09 08:00:00+00	EstUnestablished	-2	\N	["sen"]	f
\.


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_id_seq', 2, true);


--
-- Data for Name: user_message_pref; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY user_message_pref (id, "user", type, delivery) FROM stdin;
\.


--
-- Name: user_message_pref_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_message_pref_id_seq', 1, false);


--
-- Data for Name: user_notification; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY user_notification (id, created_ts, type, "to", content, archived) FROM stdin;
\.


--
-- Data for Name: user_notification_email; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY user_notification_email (id, created_ts, type, "to", content) FROM stdin;
\.


--
-- Name: user_notification_email_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_notification_email_id_seq', 1, false);


--
-- Name: user_notification_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_notification_id_seq', 1, false);


--
-- Data for Name: user_notification_pref; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY user_notification_pref (id, "user", type, delivery) FROM stdin;
\.


--
-- Name: user_notification_pref_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_notification_pref_id_seq', 1, false);


--
-- Data for Name: user_setting; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY user_setting (id, "user", setting, value) FROM stdin;
\.


--
-- Name: user_setting_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_setting_id_seq', 1, false);


--
-- Data for Name: user_watching_project; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY user_watching_project (id, "user", project) FROM stdin;
\.


--
-- Name: user_watching_project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_watching_project_id_seq', 1, false);


--
-- Data for Name: view_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY view_comment (id, "user", comment) FROM stdin;
\.


--
-- Name: view_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('view_comment_id_seq', 1, false);


--
-- Data for Name: view_time; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY view_time (id, "user", project, type, "time") FROM stdin;
\.


--
-- Name: view_time_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('view_time_id_seq', 1, false);


--
-- Data for Name: view_wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY view_wiki_edit (id, "user", edit) FROM stdin;
\.


--
-- Name: view_wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('view_wiki_edit_id_seq', 1, false);


--
-- Data for Name: volunteer_application; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY volunteer_application (id, created_ts, project, "user", name, email, other_contact_info, website, location, experience, comments) FROM stdin;
\.


--
-- Name: volunteer_application_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('volunteer_application_id_seq', 1, false);


--
-- Data for Name: volunteer_interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY volunteer_interest (id, volunteer, interest) FROM stdin;
\.


--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('volunteer_interest_id_seq', 1, false);


--
-- Data for Name: watched_subthread; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY watched_subthread (id, ts, "user", root) FROM stdin;
\.


--
-- Name: watched_subthread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('watched_subthread_id_seq', 1, false);


--
-- Data for Name: wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_edit (id, ts, "user", page, content, comment, language) FROM stdin;
1	2014-01-21 17:46:06.695166+00	1	1	# Welcome\n\nThank you for testing (and hopefully helping to develop) Snowdrift.coop!\n\nThis is a wiki page within your test database. It is different than the database for the real Snowdrift.coop site.	Page created.	en
2	2014-01-21 17:48:55.489319+00	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.	Page created.	en
3	2014-01-21 17:52:33.270443+00	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.	Added links to wiki page on live site and comment about history	en
4	2014-01-21 17:53:21.094299+00	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.\n\nThere are discussion pages for every wiki page, as shown above.	Added sentence about discussion pages	en
5	2014-01-21 17:55:07.436846+00	1	3	See the live site for [press info](https://snowdrift.coop/p/snowdrift/w/press)	Page created.	en
6	2014-01-21 18:09:53.469506+00	1	4	# Development notes\n\nSee the live site for the full [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help).\n\n## Development notes\n\nThe essential development details are in the README.md file with the code, not in this test database. When adding new info, consider whether it is best there versus here in the test database (the README has instructions about updating the test database).\n\n## Users\n\n[localhost:3000/u](/u) is a listing of all the users. The first user is just "admin" (passphrase is also "admin"). When new users register they start out unestablished and with no roles. You can add roles by using the admin user and visiting <http://localhost:3000/p/snowdrift/invite> and then logging in as another user to redeem the code.\n\nIt is a good idea to test things as:\n\na. logged-out\na. unestablished user\na. established users with different roles\n\nObviously testing on different systems, browsers, devices, etc. is good too.\n\n## Tickets\n\nSee <https://snowdrift.coop/p/snowdrift/t> for the live site's list of tickets. This is also linked at the live site's how-to-help page. Please add tickets to the live site as appropriate, add comments and questions, and mark things complete after you have fixed them and committed your changes.	Page created.	en
\.


--
-- Name: wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_edit_id_seq', 6, true);


--
-- Data for Name: wiki_last_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_last_edit (id, page, edit, language) FROM stdin;
1	1	1	en
2	2	4	en
3	3	5	en
4	4	6	en
\.


--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_last_edit_id_seq', 4, true);


--
-- Data for Name: wiki_page; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_page (id, project, permission_level, discussion, created_ts, "user") FROM stdin;
1	1	Normal	1	2015-07-14 23:55:35.574688+01	1
2	1	Normal	2	2015-07-14 23:55:35.574688+01	1
3	1	Normal	3	2015-07-14 23:55:35.574688+01	1
4	1	Normal	4	2015-07-14 23:55:35.574688+01	1
\.


--
-- Data for Name: wiki_page_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_page_comment (id, comment, page) FROM stdin;
1	1	2
2	2	2
3	3	2
4	4	2
\.


--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_page_comment_id_seq', 4, true);


--
-- Name: wiki_page_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_page_id_seq', 4, true);


--
-- Data for Name: wiki_target; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_target (id, page, project, target, language) FROM stdin;
1	1	1	intro	en
2	2	1	about	en
3	3	1	press	en
4	4	1	how-to-help	en
\.


--
-- Name: wiki_target_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_target_id_seq', 4, true);


--
-- Data for Name: wiki_translation; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_translation (id, edit, source, complete) FROM stdin;
\.


--
-- Name: wiki_translation_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_translation_id_seq', 1, false);


--
-- Name: a_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY a
    ADD CONSTRAINT a_pkey PRIMARY KEY (id);


--
-- Name: account_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: build_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY build
    ADD CONSTRAINT build_pkey PRIMARY KEY (id);


--
-- Name: comment_ancestor_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_pkey PRIMARY KEY (id);


--
-- Name: comment_closing_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_closing
    ADD CONSTRAINT comment_closing_pkey PRIMARY KEY (id);


--
-- Name: comment_flagging_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT comment_flagging_pkey PRIMARY KEY (id);


--
-- Name: comment_flagging_reason_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_flagging_reason
    ADD CONSTRAINT comment_flagging_reason_pkey PRIMARY KEY (id);


--
-- Name: comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);


--
-- Name: comment_rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_pkey PRIMARY KEY (id);


--
-- Name: comment_retracting_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_retracting
    ADD CONSTRAINT comment_retracting_pkey PRIMARY KEY (id);


--
-- Name: comment_retraction_pkey1; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_pkey1 PRIMARY KEY (id);


--
-- Name: comment_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_pkey PRIMARY KEY (id);


--
-- Name: committee_user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_pkey PRIMARY KEY (id);


--
-- Name: database_version_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY database_version
    ADD CONSTRAINT database_version_pkey PRIMARY KEY (id);


--
-- Name: default_tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_pkey PRIMARY KEY (id);


--
-- Name: delete_confirmation_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY delete_confirmation
    ADD CONSTRAINT delete_confirmation_pkey PRIMARY KEY (id);


--
-- Name: deprecated_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY deprecated_tag
    ADD CONSTRAINT deprecated_tag_pkey PRIMARY KEY (id);


--
-- Name: discussion_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY discussion
    ADD CONSTRAINT discussion_pkey PRIMARY KEY (id);


--
-- Name: doc_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_pkey PRIMARY KEY (id);


--
-- Name: doc_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_pkey PRIMARY KEY (id);


--
-- Name: email_verification_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY email_verification
    ADD CONSTRAINT email_verification_pkey PRIMARY KEY (id);


--
-- Name: event_blog_post_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_blog_post
    ADD CONSTRAINT event_blog_post_pkey PRIMARY KEY (id);


--
-- Name: event_comment_closing_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_comment_closing
    ADD CONSTRAINT event_comment_closing_pkey PRIMARY KEY (id);


--
-- Name: event_comment_pending_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_comment_pending
    ADD CONSTRAINT event_comment_pending_pkey PRIMARY KEY (id);


--
-- Name: event_comment_posted_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_comment_posted
    ADD CONSTRAINT event_comment_posted_pkey PRIMARY KEY (id);


--
-- Name: event_comment_rethreaded_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_comment_rethreaded
    ADD CONSTRAINT event_comment_rethreaded_pkey PRIMARY KEY (id);


--
-- Name: event_deleted_pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_deleted_pledge
    ADD CONSTRAINT event_deleted_pledge_pkey PRIMARY KEY (id);


--
-- Name: event_new_pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_new_pledge
    ADD CONSTRAINT event_new_pledge_pkey PRIMARY KEY (id);


--
-- Name: event_project_notification_sent_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_project_notification_sent
    ADD CONSTRAINT event_project_notification_sent_pkey PRIMARY KEY (id);


--
-- Name: event_ticket_claimed_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_ticket_claimed
    ADD CONSTRAINT event_ticket_claimed_pkey PRIMARY KEY (id);


--
-- Name: event_ticket_unclaimed_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_ticket_unclaimed
    ADD CONSTRAINT event_ticket_unclaimed_pkey PRIMARY KEY (id);


--
-- Name: event_updated_pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_updated_pledge
    ADD CONSTRAINT event_updated_pledge_pkey PRIMARY KEY (id);


--
-- Name: event_user_notification_sent_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_user_notification_sent
    ADD CONSTRAINT event_user_notification_sent_pkey PRIMARY KEY (id);


--
-- Name: event_wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_wiki_edit
    ADD CONSTRAINT event_wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: event_wiki_page_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY event_wiki_page
    ADD CONSTRAINT event_wiki_page_pkey PRIMARY KEY (id);


--
-- Name: image_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY image
    ADD CONSTRAINT image_pkey PRIMARY KEY (id);


--
-- Name: interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY interest
    ADD CONSTRAINT interest_pkey PRIMARY KEY (id);


--
-- Name: invite_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_pkey PRIMARY KEY (id);


--
-- Name: license_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY license
    ADD CONSTRAINT license_pkey PRIMARY KEY (id);


--
-- Name: manual_establishment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_pkey PRIMARY KEY (id);


--
-- Name: payday_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY payday
    ADD CONSTRAINT payday_pkey PRIMARY KEY (id);


--
-- Name: pledge_form_rendered_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY pledge_form_rendered
    ADD CONSTRAINT pledge_form_rendered_pkey PRIMARY KEY (id);


--
-- Name: pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_pkey PRIMARY KEY (id);


--
-- Name: project_blog_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_pkey PRIMARY KEY (id);


--
-- Name: project_blog_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT project_blog_pkey PRIMARY KEY (id);


--
-- Name: project_last_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_pkey PRIMARY KEY (id);


--
-- Name: project_notification_email_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_notification_email
    ADD CONSTRAINT project_notification_email_pkey PRIMARY KEY (id);


--
-- Name: project_notification_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_notification
    ADD CONSTRAINT project_notification_pkey PRIMARY KEY (id);


--
-- Name: project_notification_pref_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_notification_pref
    ADD CONSTRAINT project_notification_pref_pkey PRIMARY KEY (id);


--
-- Name: project_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_pkey PRIMARY KEY (id);


--
-- Name: project_signup_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_signup
    ADD CONSTRAINT project_signup_pkey PRIMARY KEY (id);


--
-- Name: project_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_pkey PRIMARY KEY (id);


--
-- Name: project_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_pkey PRIMARY KEY (id);


--
-- Name: project_user_role_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_pkey PRIMARY KEY (id);


--
-- Name: reset_password_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY reset_password
    ADD CONSTRAINT reset_password_pkey PRIMARY KEY (id);


--
-- Name: rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY rethread
    ADD CONSTRAINT rethread_pkey PRIMARY KEY (id);


--
-- Name: role_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_pkey PRIMARY KEY (id);


--
-- Name: shares_pledged_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_pkey PRIMARY KEY (id);


--
-- Name: tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_pkey PRIMARY KEY (id);


--
-- Name: tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_pkey PRIMARY KEY (id);


--
-- Name: ticket_claiming_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY ticket_claiming
    ADD CONSTRAINT ticket_claiming_pkey PRIMARY KEY (id);


--
-- Name: ticket_old_claiming_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY ticket_old_claiming
    ADD CONSTRAINT ticket_old_claiming_pkey PRIMARY KEY (id);


--
-- Name: ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_pkey PRIMARY KEY (id);


--
-- Name: transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);


--
-- Name: unapproved_comment_notification_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY unapproved_comment_notification
    ADD CONSTRAINT unapproved_comment_notification_pkey PRIMARY KEY (id);


--
-- Name: unique_blog_post; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT unique_blog_post UNIQUE (project, handle);


--
-- Name: unique_blog_post_discussion; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT unique_blog_post_discussion UNIQUE (discussion);


--
-- Name: unique_comment_ancestor; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT unique_comment_ancestor UNIQUE (comment, ancestor);


--
-- Name: unique_comment_closing; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_closing
    ADD CONSTRAINT unique_comment_closing UNIQUE (comment);


--
-- Name: unique_comment_flagging; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT unique_comment_flagging UNIQUE (comment);


--
-- Name: unique_comment_flagging_reason; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_flagging_reason
    ADD CONSTRAINT unique_comment_flagging_reason UNIQUE (flagging, reason);


--
-- Name: unique_comment_retracting; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_retracting
    ADD CONSTRAINT unique_comment_retracting UNIQUE (comment);


--
-- Name: unique_comment_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT unique_comment_tag UNIQUE (comment, tag, "user");


--
-- Name: unique_committee_member; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT unique_committee_member UNIQUE ("user");


--
-- Name: unique_default_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT unique_default_tag UNIQUE (tag);


--
-- Name: unique_delete_confirmation; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY delete_confirmation
    ADD CONSTRAINT unique_delete_confirmation UNIQUE ("user", email, uri);


--
-- Name: unique_doc_name; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT unique_doc_name UNIQUE (name);


--
-- Name: unique_email_verification; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY email_verification
    ADD CONSTRAINT unique_email_verification UNIQUE ("user", email, uri);


--
-- Name: unique_image_handle; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY image
    ADD CONSTRAINT unique_image_handle UNIQUE (name);


--
-- Name: unique_invite; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT unique_invite UNIQUE (code);


--
-- Name: unique_manual_establishment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT unique_manual_establishment UNIQUE (established_user);


--
-- Name: unique_password_reset; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY reset_password
    ADD CONSTRAINT unique_password_reset UNIQUE ("user", email, uri);


--
-- Name: unique_pledge; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT unique_pledge UNIQUE ("user", project);


--
-- Name: unique_project_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_account UNIQUE (account);


--
-- Name: unique_project_blog_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT unique_project_blog_comment UNIQUE (comment);


--
-- Name: unique_project_discussion; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_discussion UNIQUE (discussion);


--
-- Name: unique_project_handle; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_handle UNIQUE (handle);


--
-- Name: unique_project_last_update; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT unique_project_last_update UNIQUE (project);


--
-- Name: unique_project_notification; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_notification
    ADD CONSTRAINT unique_project_notification UNIQUE (created_ts, type, "to", project);


--
-- Name: unique_project_notification_email; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_notification_email
    ADD CONSTRAINT unique_project_notification_email UNIQUE (created_ts, type, "to", project);


--
-- Name: unique_project_notification_pref; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_notification_pref
    ADD CONSTRAINT unique_project_notification_pref UNIQUE ("user", project, type);


--
-- Name: unique_project_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT unique_project_tag UNIQUE (project, tag);


--
-- Name: unique_project_user_role; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT unique_project_user_role UNIQUE (project, "user", role);


--
-- Name: unique_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT unique_tag UNIQUE (name);


--
-- Name: unique_tag_color; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT unique_tag_color UNIQUE (tag, "user");


--
-- Name: unique_ticket; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT unique_ticket UNIQUE (comment);


--
-- Name: unique_ticket_claiming; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY ticket_claiming
    ADD CONSTRAINT unique_ticket_claiming UNIQUE (ticket);


--
-- Name: unique_user; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user UNIQUE (ident);


--
-- Name: unique_user_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user_account UNIQUE (account);


--
-- Name: unique_user_discussion; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user_discussion UNIQUE (discussion);


--
-- Name: unique_user_message_pref; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_message_pref
    ADD CONSTRAINT unique_user_message_pref UNIQUE ("user", type);


--
-- Name: unique_user_notification; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_notification
    ADD CONSTRAINT unique_user_notification UNIQUE (created_ts, type, "to");


--
-- Name: unique_user_notification_email; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_notification_email
    ADD CONSTRAINT unique_user_notification_email UNIQUE (created_ts, type, "to");


--
-- Name: unique_user_notification_pref; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_notification_pref
    ADD CONSTRAINT unique_user_notification_pref UNIQUE ("user", type);


--
-- Name: unique_user_watching_project; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT unique_user_watching_project UNIQUE ("user", project);


--
-- Name: unique_view_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT unique_view_comment UNIQUE ("user", comment);


--
-- Name: unique_view_time_user_project_type; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT unique_view_time_user_project_type UNIQUE ("user", project, type);


--
-- Name: unique_view_wiki_edit; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT unique_view_wiki_edit UNIQUE ("user", edit);


--
-- Name: unique_wiki_last_edit; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT unique_wiki_last_edit UNIQUE (page, language);


--
-- Name: unique_wiki_page_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT unique_wiki_page_comment UNIQUE (comment);


--
-- Name: unique_wiki_page_discussion; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT unique_wiki_page_discussion UNIQUE (discussion);


--
-- Name: unique_wiki_target; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_target
    ADD CONSTRAINT unique_wiki_target UNIQUE (project, language, target);


--
-- Name: unnamed_image_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY unnamed_image
    ADD CONSTRAINT unnamed_image_pkey PRIMARY KEY (id);


--
-- Name: user_message_pref_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_message_pref
    ADD CONSTRAINT user_message_pref_pkey PRIMARY KEY (id);


--
-- Name: user_notification_email_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_notification_email
    ADD CONSTRAINT user_notification_email_pkey PRIMARY KEY (id);


--
-- Name: user_notification_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_notification
    ADD CONSTRAINT user_notification_pkey PRIMARY KEY (id);


--
-- Name: user_notification_pref_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_notification_pref
    ADD CONSTRAINT user_notification_pref_pkey PRIMARY KEY (id);


--
-- Name: user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: user_setting_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_pkey PRIMARY KEY (id);


--
-- Name: user_watching_project_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT user_watching_project_pkey PRIMARY KEY (id);


--
-- Name: view_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT view_comment_pkey PRIMARY KEY (id);


--
-- Name: view_time_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_pkey PRIMARY KEY (id);


--
-- Name: view_wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT view_wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: volunteer_application_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_pkey PRIMARY KEY (id);


--
-- Name: volunteer_interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_pkey PRIMARY KEY (id);


--
-- Name: watched_subthread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY watched_subthread
    ADD CONSTRAINT watched_subthread_pkey PRIMARY KEY (id);


--
-- Name: wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_last_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_pkey PRIMARY KEY (id);


--
-- Name: wiki_target_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_target
    ADD CONSTRAINT wiki_target_pkey PRIMARY KEY (id);


--
-- Name: wiki_translation_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_translation
    ADD CONSTRAINT wiki_translation_pkey PRIMARY KEY (id);


--
-- Name: doc_event; Type: TRIGGER; Schema: public; Owner: snowdrift_test
--

CREATE TRIGGER doc_event AFTER INSERT OR DELETE ON doc FOR EACH ROW EXECUTE PROCEDURE log_doc_event_trigger();


--
-- Name: role_event; Type: TRIGGER; Schema: public; Owner: snowdrift_test
--

CREATE TRIGGER role_event AFTER INSERT OR DELETE ON project_user_role FOR EACH ROW EXECUTE PROCEDURE log_role_event_trigger();


--
-- Name: blog_post_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT blog_post_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: blog_post_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT blog_post_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: blog_post_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT blog_post_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: comment_ancestor_ancestor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_ancestor_fkey FOREIGN KEY (ancestor) REFERENCES comment(id);


--
-- Name: comment_ancestor_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_approved_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_approved_by_fkey FOREIGN KEY (approved_by) REFERENCES "user"(id);


--
-- Name: comment_closing_closed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_closing
    ADD CONSTRAINT comment_closing_closed_by_fkey FOREIGN KEY (closed_by) REFERENCES "user"(id);


--
-- Name: comment_closing_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_closing
    ADD CONSTRAINT comment_closing_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: comment_flagging_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT comment_flagging_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_flagging_flagger_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_flagging
    ADD CONSTRAINT comment_flagging_flagger_fkey FOREIGN KEY (flagger) REFERENCES "user"(id);


--
-- Name: comment_flagging_reason_flagging_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_flagging_reason
    ADD CONSTRAINT comment_flagging_reason_flagging_fkey FOREIGN KEY (flagging) REFERENCES comment_flagging(id);


--
-- Name: comment_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_by_fkey FOREIGN KEY (approved_by) REFERENCES "user"(id);


--
-- Name: comment_moderated_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_user_fkey FOREIGN KEY (approved_by) REFERENCES "user"(id);


--
-- Name: comment_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_parent_fkey FOREIGN KEY (parent) REFERENCES comment(id);


--
-- Name: comment_rethread_new_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_new_comment_fkey FOREIGN KEY (new_comment) REFERENCES comment(id);


--
-- Name: comment_rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);


--
-- Name: comment_rethread_rethread_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_rethread_fkey FOREIGN KEY (rethread) REFERENCES rethread(id);


--
-- Name: comment_retracting_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_retracting
    ADD CONSTRAINT comment_retracting_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: comment_tag_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: comment_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: committee_user_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: committee_user_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: default_tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: delete_confirmation_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY delete_confirmation
    ADD CONSTRAINT delete_confirmation_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: deprecated_tag_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY deprecated_tag
    ADD CONSTRAINT deprecated_tag_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: deprecated_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY deprecated_tag
    ADD CONSTRAINT deprecated_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: doc_current_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_current_version_fkey FOREIGN KEY (current_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_blessed_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_blessed_version_fkey FOREIGN KEY (blessed_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_doc_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_doc_fkey FOREIGN KEY (doc) REFERENCES doc(id);


--
-- Name: email_verification_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY email_verification
    ADD CONSTRAINT email_verification_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: event_blog_post_post_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_blog_post
    ADD CONSTRAINT event_blog_post_post_fkey FOREIGN KEY (post) REFERENCES blog_post(id);


--
-- Name: event_comment_closing_comment_closing_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_closing
    ADD CONSTRAINT event_comment_closing_comment_closing_fkey FOREIGN KEY (comment_closing) REFERENCES comment_closing(id);


--
-- Name: event_comment_pending_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_pending
    ADD CONSTRAINT event_comment_pending_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: event_comment_posted_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_posted
    ADD CONSTRAINT event_comment_posted_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: event_comment_rethreaded_rethread_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_comment_rethreaded
    ADD CONSTRAINT event_comment_rethreaded_rethread_fkey FOREIGN KEY (rethread) REFERENCES rethread(id);


--
-- Name: event_deleted_pledge_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_deleted_pledge
    ADD CONSTRAINT event_deleted_pledge_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: event_deleted_pledge_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_deleted_pledge
    ADD CONSTRAINT event_deleted_pledge_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: event_new_pledge_shares_pledged_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_new_pledge
    ADD CONSTRAINT event_new_pledge_shares_pledged_fkey FOREIGN KEY (shares_pledged) REFERENCES shares_pledged(id);


--
-- Name: event_project_notification_sent_notification_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_project_notification_sent
    ADD CONSTRAINT event_project_notification_sent_notification_fkey FOREIGN KEY (notification) REFERENCES project_notification(id);


--
-- Name: event_ticket_claimed_claim_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_ticket_claimed
    ADD CONSTRAINT event_ticket_claimed_claim_fkey FOREIGN KEY (claim) REFERENCES ticket_claiming(id);


--
-- Name: event_ticket_claimed_old_claim_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_ticket_claimed
    ADD CONSTRAINT event_ticket_claimed_old_claim_fkey FOREIGN KEY (old_claim) REFERENCES ticket_old_claiming(id);


--
-- Name: event_ticket_unclaimed_claim_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_ticket_unclaimed
    ADD CONSTRAINT event_ticket_unclaimed_claim_fkey FOREIGN KEY (claim) REFERENCES ticket_old_claiming(id);


--
-- Name: event_updated_pledge_shares_pledged_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_updated_pledge
    ADD CONSTRAINT event_updated_pledge_shares_pledged_fkey FOREIGN KEY (shares_pledged) REFERENCES shares_pledged(id);


--
-- Name: event_user_notification_sent_notification_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_user_notification_sent
    ADD CONSTRAINT event_user_notification_sent_notification_fkey FOREIGN KEY (notification) REFERENCES user_notification(id);


--
-- Name: event_wiki_edit_wiki_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_wiki_edit
    ADD CONSTRAINT event_wiki_edit_wiki_edit_fkey FOREIGN KEY (wiki_edit) REFERENCES wiki_edit(id);


--
-- Name: event_wiki_page_wiki_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY event_wiki_page
    ADD CONSTRAINT event_wiki_page_wiki_page_fkey FOREIGN KEY (wiki_page) REFERENCES wiki_page(id);


--
-- Name: image_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY image
    ADD CONSTRAINT image_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: image_uploader_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY image
    ADD CONSTRAINT image_uploader_fkey FOREIGN KEY (uploader) REFERENCES "user"(id);


--
-- Name: invite_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: invite_redeemed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_redeemed_by_fkey FOREIGN KEY (redeemed_by) REFERENCES "user"(id);


--
-- Name: invite_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: manual_establishment_established_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_established_user_fkey FOREIGN KEY (established_user) REFERENCES "user"(id);


--
-- Name: manual_establishment_establishing_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_establishing_user_fkey FOREIGN KEY (establishing_user) REFERENCES "user"(id);


--
-- Name: pledge_form_rendered_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge_form_rendered
    ADD CONSTRAINT pledge_form_rendered_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: pledge_form_rendered_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge_form_rendered
    ADD CONSTRAINT pledge_form_rendered_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: pledge_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: pledge_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: project_blog_comment_blog_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_blog_fkey FOREIGN KEY (blog) REFERENCES blog_post(id);


--
-- Name: project_blog_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: project_blog_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT project_blog_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: project_blog_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT project_blog_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_blog_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY blog_post
    ADD CONSTRAINT project_blog_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: project_last_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_last_payday_fkey FOREIGN KEY (last_payday) REFERENCES payday(id);


--
-- Name: project_last_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_last_update_update_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_update_fkey FOREIGN KEY (update) REFERENCES project_update(id);


--
-- Name: project_notification_email_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification_email
    ADD CONSTRAINT project_notification_email_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_notification_email_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification_email
    ADD CONSTRAINT project_notification_email_to_fkey FOREIGN KEY ("to") REFERENCES "user"(id);


--
-- Name: project_notification_pref_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification_pref
    ADD CONSTRAINT project_notification_pref_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_notification_pref_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification_pref
    ADD CONSTRAINT project_notification_pref_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_notification_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification
    ADD CONSTRAINT project_notification_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_notification_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_notification
    ADD CONSTRAINT project_notification_to_fkey FOREIGN KEY ("to") REFERENCES "user"(id);


--
-- Name: project_tag_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: project_update_author_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_author_fkey FOREIGN KEY (author) REFERENCES "user"(id);


--
-- Name: project_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: reset_password_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY reset_password
    ADD CONSTRAINT reset_password_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY rethread
    ADD CONSTRAINT rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);


--
-- Name: rethread_new_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY rethread
    ADD CONSTRAINT rethread_new_comment_fkey FOREIGN KEY (new_comment) REFERENCES comment(id);


--
-- Name: rethread_old_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY rethread
    ADD CONSTRAINT rethread_old_comment_fkey FOREIGN KEY (old_comment) REFERENCES comment(id);


--
-- Name: role_event_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: role_event_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: shares_pledged_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: shares_pledged_render_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_render_fkey FOREIGN KEY (render) REFERENCES pledge_form_rendered(id);


--
-- Name: shares_pledged_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY shares_pledged
    ADD CONSTRAINT shares_pledged_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: tag_color_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: ticket_claiming_ticket_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket_claiming
    ADD CONSTRAINT ticket_claiming_ticket_fkey FOREIGN KEY (ticket) REFERENCES comment(id);


--
-- Name: ticket_claiming_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket_claiming
    ADD CONSTRAINT ticket_claiming_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: ticket_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: ticket_old_claiming_ticket_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket_old_claiming
    ADD CONSTRAINT ticket_old_claiming_ticket_fkey FOREIGN KEY (ticket) REFERENCES comment(id);


--
-- Name: ticket_old_claiming_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket_old_claiming
    ADD CONSTRAINT ticket_old_claiming_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: transaction_credit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_credit_fkey FOREIGN KEY (credit) REFERENCES account(id);


--
-- Name: transaction_debit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_debit_fkey FOREIGN KEY (debit) REFERENCES account(id);


--
-- Name: transaction_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_payday_fkey FOREIGN KEY (payday) REFERENCES payday(id);


--
-- Name: unapproved_comment_notification_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY unapproved_comment_notification
    ADD CONSTRAINT unapproved_comment_notification_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: unapproved_comment_notification_notification_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY unapproved_comment_notification
    ADD CONSTRAINT unapproved_comment_notification_notification_fkey FOREIGN KEY (notification) REFERENCES user_notification(id);


--
-- Name: unnamed_image_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY unnamed_image
    ADD CONSTRAINT unnamed_image_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: unnamed_image_uploader_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY unnamed_image
    ADD CONSTRAINT unnamed_image_uploader_fkey FOREIGN KEY (uploader) REFERENCES "user"(id);


--
-- Name: user_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: user_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: user_message_pref_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_message_pref
    ADD CONSTRAINT user_message_pref_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: user_notification_email_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_notification_email
    ADD CONSTRAINT user_notification_email_to_fkey FOREIGN KEY ("to") REFERENCES "user"(id);


--
-- Name: user_notification_pref_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_notification_pref
    ADD CONSTRAINT user_notification_pref_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: user_notification_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_notification
    ADD CONSTRAINT user_notification_to_fkey FOREIGN KEY ("to") REFERENCES "user"(id);


--
-- Name: user_setting_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: user_watching_project_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT user_watching_project_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: user_watching_project_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_watching_project
    ADD CONSTRAINT user_watching_project_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: view_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT view_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: view_comment_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_comment
    ADD CONSTRAINT view_comment_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: view_time_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: view_time_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: view_wiki_edit_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT view_wiki_edit_edit_fkey FOREIGN KEY (edit) REFERENCES wiki_edit(id);


--
-- Name: view_wiki_edit_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_wiki_edit
    ADD CONSTRAINT view_wiki_edit_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_application_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: volunteer_application_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_interest_interest_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_interest_fkey FOREIGN KEY (interest) REFERENCES interest(id);


--
-- Name: volunteer_interest_volunteer_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_volunteer_fkey FOREIGN KEY (volunteer) REFERENCES volunteer_application(id);


--
-- Name: watched_subthread_root_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY watched_subthread
    ADD CONSTRAINT watched_subthread_root_fkey FOREIGN KEY (root) REFERENCES comment(id);


--
-- Name: watched_subthread_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY watched_subthread
    ADD CONSTRAINT watched_subthread_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: wiki_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_edit_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: wiki_last_edit_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_edit_fkey FOREIGN KEY (edit) REFERENCES wiki_edit(id);


--
-- Name: wiki_last_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: wiki_page_comment_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: wiki_page_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: wiki_page_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: wiki_target_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_target
    ADD CONSTRAINT wiki_target_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_target_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_target
    ADD CONSTRAINT wiki_target_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: wiki_translation_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_translation
    ADD CONSTRAINT wiki_translation_edit_fkey FOREIGN KEY (edit) REFERENCES wiki_edit(id);


--
-- Name: wiki_translation_source_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_translation
    ADD CONSTRAINT wiki_translation_source_fkey FOREIGN KEY (source) REFERENCES wiki_edit(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

