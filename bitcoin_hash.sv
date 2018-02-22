module bitcoin_hash (input logic clk, reset_n, start,
 input logic [31:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);
 
 
assign mem_clk = clk;
  parameter int sha256_k[0:63] = '{
 32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
 32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
 32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
 32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
 32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
 32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
 32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
 32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};
enum logic [3:0] {IDLE=4'b0000, PRO_READ=4'b0001, PRE_COMPUTE_FIRST_BLOCK=4'b0011, 
 COMPUTE_FIRST_BLOCK=4'b0100, DONE=4'b0101, PRE_COMPUTE_SECOND_BLOCK=4'B0110, COMPUTE_SECOND_BLOCK=4'b0111, 
 UPDATE_FIRST_HASH=4'b1000, UPDATE_SECOND_HASH = 4'b1001, COMPUTE_SECOND_ROUND_HASH=4'b1011,
 PRE_COMPUTE_SECOND_ROUND_HASH=4'b1101, UPDATE_FINAL_HASH=4'b1110, WRITE=4'b1010} state;

  logic [15:0] rc; // read and write counters during read state
  logic [15:0] out_wc;
  logic [7:0] t; // t ranges from 0 to 63


parameter NUM_NONCES = 16;
logic   [31:0] h0[NUM_NONCES];
logic   [31:0] h1[NUM_NONCES];
logic   [31:0] h2[NUM_NONCES];
logic   [31:0] h3[NUM_NONCES];
logic   [31:0] h4[NUM_NONCES];
logic   [31:0] h5[NUM_NONCES];
logic   [31:0] h6[NUM_NONCES];
logic   [31:0] h7[NUM_NONCES];

logic [31:0] fh0, fh1, fh2, fh3, fh4, fh5, fh6, fh7; // first h value. assigned when start

logic [31:0] A[NUM_NONCES];
logic [31:0] B[NUM_NONCES];
logic [31:0] C[NUM_NONCES];
logic [31:0] D[NUM_NONCES];
logic [31:0] E[NUM_NONCES];
logic [31:0] F[NUM_NONCES];
logic [31:0] G[NUM_NONCES];
logic [31:0] H[NUM_NONCES];
logic [31:0] p[NUM_NONCES];
logic[31:0] w[NUM_NONCES][0:15];
logic [31:0] word_17, word_18, word_19;
int i;

function logic [31:0] rrot(input logic[31:0] oldW, input logic[7:0] r);
begin	
	rrot = ((oldW << (32-r)) | (oldW>>r));
end
endfunction

function logic [31:0] wtnew(input logic[31:0] nc); // function with no inputs
 logic [31:0] sa, sb;
 sa = rrot(w[nc][1],7)^rrot(w[nc][1],18)^(w[nc][1]>>3);
 sb = rrot(w[nc][14],17)^rrot(w[nc][14],19)^(w[nc][14]>>10);
 wtnew = w[nc][0] + sa + w[nc][9] + sb;
endfunction


function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w, p);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rrot(e, 6) ^ rrot(e, 11) ^ rrot(e, 25);
    ch = (e & f) ^ ((~e) & g);
    //t1 = S1 + ch + sha256_k[t] + w + h;
	 t1 = S1 + ch + p;
    S0 = rrot(a, 2) ^ rrot(a, 13) ^ rrot(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction 


 
 
 // start always_ff here
always_ff @(posedge clk, negedge reset_n)
  begin
    if (!reset_n) begin
      state <= IDLE;
			done <= 0;
    end else
      case (state)
      IDLE: // start
        if (start) begin 
			  // global setup goes here. Not per Block variables
				state <= PRO_READ;
				out_wc <= 0;
				fh0 <= 32'h6a09e667;
				fh1 <= 32'hbb67ae85;
				fh2 <= 32'h3c6ef372;
				fh3 <= 32'ha54ff53a;
				fh4 <= 32'h510e527f;
				fh5 <= 32'h9b05688c;
				fh6 <= 32'h1f83d9ab;
				fh7 <= 32'h5be0cd19;
				// read first word here
				rc <= 1;
				mem_we <= 0;
				mem_addr <= message_addr;
        end
		PRO_READ:
			// init A to H. request to read the second word
			begin
				//$display("PRO_READ");
				t <= 0;
				// only initialize A[0] ... H[0]
				A[0] <= fh0;
				B[0] <= fh1;
				C[0] <= fh2;
				D[0] <= fh3;
				E[0] <= fh4;
				F[0] <= fh5;
				G[0] <= fh6;
				H[0] <= fh7;
				mem_we <= 0;
				rc <= rc + 16'd1;
				mem_addr <= message_addr + rc;
				state <= PRE_COMPUTE_FIRST_BLOCK;
				
			end
		
		PRE_COMPUTE_FIRST_BLOCK:
			// only handle the first cycle.
			
			begin
				w[0][0] <= mem_read_data;
				p[0] <= H[0] + sha256_k[0] + mem_read_data;
				t <= t + 8'd1;
				mem_we <= 0;
				rc <= rc + 16'd1;
				mem_addr <= message_addr + rc;
				state <= COMPUTE_FIRST_BLOCK;
			end
		COMPUTE_FIRST_BLOCK:
		
		begin
			//$display("COMPUTE_FIRST_BLOCK state");
			if (t == 64) begin
				// done with this round, go back to proread
				{A[0], B[0], C[0], D[0], E[0], F[0], G[0], H[0]} <= sha256_op(A[0], B[0], C[0], D[0], E[0], F[0], G[0], H[0], w[0][15], p[0]);
				t <= 8'd0;
				state <= UPDATE_FIRST_HASH;
				
			end else begin
				// do on cycle of execution.
				if (t < 16) begin 
					{A[0], B[0], C[0], D[0], E[0], F[0], G[0], H[0]} <= sha256_op(A[0], B[0], C[0], D[0], E[0], F[0], G[0], H[0], w[0][t-1], p[0]);
					w[0][t] <= mem_read_data;
					p[0] <= G[0] + sha256_k[t] + mem_read_data;
					mem_we <= 0;
					rc <= rc + 16'd1;
					mem_addr <= message_addr + rc;
				end else begin
					{A[0], B[0], C[0], D[0], E[0], F[0], G[0], H[0]} <= sha256_op(A[0], B[0], C[0], D[0], E[0], F[0], G[0], H[0], w[0][15], p[0]);
					for (i = 0; i < 15; i=i+1) begin
						w[0][i] <= w[0][i+1];
					end 
					w[0][15] <= wtnew(0);
					p[0] <= G[0] + sha256_k[t] +wtnew(0);
					//{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, wtnew(), t);
					if (t == 16) begin
						mem_we <= 0;
						mem_addr <= message_addr + rc;
						rc <= rc + 16'd1;
						word_17 <= mem_read_data;
					end else if (t == 17) begin
						word_18 <= mem_read_data;
					end else if (t == 18) begin
						word_19 <= mem_read_data;
						rc <= 16'd0;
					end
				end
				t <= t + 8'd1;
				state <= COMPUTE_FIRST_BLOCK;
				//state <= UPDATE_PARAM;
				//state <= UPDATE_S;
			end // END else
		end // end COMPUTE_FIRST_BLOCK

		UPDATE_FIRST_HASH:
			begin
				//$display("UPDATE_HASH state");
				fh0 <= fh0 + A[0];
				fh1 <= fh1 + B[0];
				fh2 <= fh2 + C[0];
				fh3 <= fh3 + D[0];
				fh4 <= fh4 + E[0];
				fh5 <= fh5 + F[0];
				fh6 <= fh6 + G[0];
				fh7 <= fh7 + H[0];
				state <= PRE_COMPUTE_SECOND_BLOCK;
			end
			
		PRE_COMPUTE_SECOND_BLOCK:
		begin
			for(i=0; i<NUM_NONCES; i++) begin
				w[i][0] <= word_17;
				//p[i] <= H[i] + sha256_k[0] + word_17;
				p[i] <= fh7 + sha256_k[0] + word_17;
				A[i] <= fh0;
				B[i] <= fh1;
				C[i] <= fh2;
				D[i] <= fh3;
				E[i] <= fh4;
				F[i] <= fh5;
				G[i] <= fh6;
				H[i] <= fh7;
				// now you did not update h0[i] ... h7[i]
			end
			t <= t + 8'd1;
			state <= COMPUTE_SECOND_BLOCK;
		end
		
		COMPUTE_SECOND_BLOCK:
		begin
			//$display("compute second block");
			if(t == 64) begin
				for(i=0; i<NUM_NONCES; i++) begin
					{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][15], p[i]);
				end
				t <= 8'd0;
				state <= UPDATE_SECOND_HASH;
			end else begin
				// t is smaller than 64. TODO.
				// do on cycle of execution.
				if (t < 16)
					
					case(t)
						1: begin
							// word_18
							for (i=0; i<NUM_NONCES; i++) begin
							{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
							w[i][t] <= word_18;
							p[i] <= G[i] + sha256_k[t] + word_18;
							end
						end
						2: begin
							// word_19
							for (i=0; i<NUM_NONCES; i++) begin
							{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
							w[i][t] <= word_19;
							p[i] <= G[i] + sha256_k[t] + word_19;
							end
						end
						3: begin // nonce. from 0 to 15.
							for (i=0; i<NUM_NONCES; i++) begin
							{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
							w[i][t] <= i;
							p[i] <= G[i] + sha256_k[t] + i;
							end
						end
						4: begin //padding 32'h80000000
							for (i=0; i<NUM_NONCES; i++) begin
							{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
							w[i][t] <= 32'h80000000;
							p[i] <= G[i] + sha256_k[t] + 32'h80000000;
							end
						end
						15: begin // size is 32'd640
							for (i=0; i<NUM_NONCES; i++) begin
							{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
							w[i][t] <= 32'd640;
							p[i] <= G[i] + sha256_k[t] + 32'd640;
							end
						end
						default: begin
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= 32'd0;
								p[i] <= G[i] + sha256_k[t] + 32'd0;
							end
						end
					endcase
					
				else begin
					for(i=0;i<NUM_NONCES;i++) begin
						{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][15], p[i]);
						for (int j = 0; j < 15; j=j+1) begin
							w[i][j] <= w[i][j+1];
						end 
						w[i][15] <= wtnew(i);
						p[i] <= G[i] + sha256_k[t] +wtnew(i);
						//{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, wtnew(), t);
					end
				end
				t <= t + 8'd1;
				state <= COMPUTE_SECOND_BLOCK;
			end
		end
		
		UPDATE_SECOND_HASH:
		begin
			//$display("update second hash");
			// update hash value
			for(i=0;i<NUM_NONCES;i++) begin
				h0[i] <= fh0 + A[i];
				h1[i] <= fh1 + B[i];
				h2[i] <= fh2 + C[i];
				h3[i] <= fh3 + D[i];
				h4[i] <= fh4 + E[i];
				h5[i] <= fh5 + F[i];
				h6[i] <= fh6 + G[i];
				h7[i] <= fh7 + H[i];
				// reset A to default value.
			end

			// update fh0 to original value and get ready for the second round sha-256
			fh0 <= 32'h6a09e667;
			fh1 <= 32'hbb67ae85;
			fh2 <= 32'h3c6ef372;
			fh3 <= 32'ha54ff53a;
			fh4 <= 32'h510e527f;
			fh5 <= 32'h9b05688c;
			fh6 <= 32'h1f83d9ab;
			fh7 <= 32'h5be0cd19;
			
			state <= PRE_COMPUTE_SECOND_ROUND_HASH;
		end

		PRE_COMPUTE_SECOND_ROUND_HASH:
		begin
			for(i=0; i<NUM_NONCES; i++) begin
					w[i][0] <= h0[i];
					//p[i] <= H[i] + sha256_k[0] + word_17;
					p[i] <= fh7 + sha256_k[0] + h0[i];
					A[i] <= fh0;
					B[i] <= fh1;
					C[i] <= fh2;
					D[i] <= fh3;
					E[i] <= fh4;
					F[i] <= fh5;
					G[i] <= fh6;
					H[i] <= fh7;
			end
			t <= 1;
			state <= COMPUTE_SECOND_ROUND_HASH;
		end

		COMPUTE_SECOND_ROUND_HASH:
		begin
			//$display("compute second round");
			// TODO: first seperate case into t < 16, t == 64 and else, then use switch to handle t < 16.
			// for the data, the first 8 come from h0...h7, the 9th is 0x10000000, and the last is size.
			if(t == 64) begin
				for(i=0; i<NUM_NONCES; i++) begin
					{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][15], p[i]);
				end
				t <= 0;
				state <= UPDATE_FINAL_HASH;
			end else begin
				// t is smaller than 64. TODO.
				// do on cycle of execution.
				if (t < 16)
					case(t)
						1: begin
							// h1[i]
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= h1[i];
								p[i] <= G[i] + sha256_k[t] + h1[i];
							end
						end
						2: begin
							// h2[i]
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= h2[i];
								p[i] <= G[i] + sha256_k[t] + h2[i];
							end
						end
						3: begin // h3[i]
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= h3[i];
								p[i] <= G[i] + sha256_k[t] + h3[i];
							end
						end
						4: begin //h4[i]
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= h4[i];
								p[i] <= G[i] + sha256_k[t] + h4[i];
							end
						end
						5:	begin //h5[i]
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= h5[i];
								p[i] <= G[i] + sha256_k[t] + h5[i];
							end
						end
						6: begin
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= h6[i];
								p[i] <= G[i] + sha256_k[t] + h6[i];
							end
						end //h6[i]
						7: begin //h7[i]
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= h7[i];
								p[i] <= G[i] + sha256_k[t] + h7[i];
							end
						end 
						8: begin // padding 32'h80000000
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= 32'h80000000;
								p[i] <= G[i] + sha256_k[t] + 32'h80000000;
							end
						end
						15: begin // size is 32'd640
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= 32'd256;
								p[i] <= G[i] + sha256_k[t] + 32'd256;
							end
						end
						default: begin
							for (i=0; i<NUM_NONCES; i++) begin
								{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][t-1], p[i]);
								w[i][t] <= 32'd0;
								p[i] <= G[i] + sha256_k[t] + 32'd0;
							end
						end
					endcase
					
				else begin
					for(i=0;i<NUM_NONCES;i++) begin
						{A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i]} <= sha256_op(A[i], B[i], C[i], D[i], E[i], F[i], G[i], H[i], w[i][15], p[i]);
						
					for (int j = 0; j < 15; j=j+1) begin
						w[i][j] <= w[i][j+1];
					end 
					w[i][15] <= wtnew(i);
					p[i] <= G[i] + sha256_k[t] +wtnew(i);
					//{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, wtnew(), t);
					end
				end
				t <= t + 8'd1;
				state <= COMPUTE_SECOND_ROUND_HASH;
			end
		end
		
		UPDATE_FINAL_HASH:
		begin
			//$display("update final hash");
			for(i=0;i<NUM_NONCES;i++) begin
				h0[i] <= fh0 + A[i];
				// reset A to default value.
				h1[i] <= fh1 + B[i];
				h2[i] <= fh2 + C[i];
				h3[i] <= fh3 + D[i];
				h4[i] <= fh4 + E[i];
				h5[i] <= fh5 + F[i];
				h6[i] <= fh6 + G[i];
				h7[i] <= fh7 + H[i];

				
			end
			state <= WRITE;
		end
		
		WRITE:
		begin
			//$display("write");
			mem_we <= 1;
			mem_addr <= output_addr + out_wc;
			mem_write_data <= h0[out_wc];
			 
			// write H0 to H7 into memory
			if(out_wc < 16) begin
				out_wc <= out_wc + 1;
				state <= WRITE;
			end else begin 
				state <= DONE;
			end
		end

		DONE:
		begin
			done <= 1;
			state <= IDLE;
		end
			endcase
  end // end always_ff begin 
 
 endmodule