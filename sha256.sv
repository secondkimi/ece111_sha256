module sha256(input logic clk, reset_n, start,
 input logic [31:0] message_addr, size, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);
 	
 enum logic [3:0] {IDLE=4'b0000, PRO_READ=4'b0001, SET_ONLY=4'b0010, READ_AND_SET=4'b0011, 
 READ_ONLY=4'b0100, DONE=4'b0101, PADDING=4'B0110, COMPUTE_W=4'b0111, UPDATE_HASH=4'b1000,
 UPDATE_PARAM=4'b1001,WRITE=4'b1010, UPDATE_S=4'b1011, PRE_COMPUTE_W=4'b1100} state;

  logic [15:0] rc, wc; // read and write counters during read state
  logic [15:0] out_wc;
  
  logic [31:0] blk_counter; // varies from 0 to (# of blocks - 1)
  logic[31:0] num_blks;// assigned by the function call 
  //logic [31:0] S0, s1, maj, t1, t2, ch;
  logic [7:0] t; // t varies from 0 to 63

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

/*logic [31:0] h[0:7];*/
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;


logic [31:0] A, B, C, D, E, F, G, H, p; 
/*logic [31:0] B; 
logic [31:0] C; 
logic [31:0] D; 
logic [31:0] E; 
logic [31:0] F; 
logic [31:0] G; 
logic [31:0] H;
logic [31:0] p;*/
//logic [31:0] S1, S0, ch, maj, t1, t2, p;

logic [31:0] M[0:15];
logic is_padding_block;
logic is_exact_block;
logic[31:0] pad_start;
int i;

logic   [ 31:0] s1, s0;
//logic   [ 31:0] w[0:63];
logic[31:0] w[0:15];

function logic [31:0] wtnew; // function with no inputs
 logic [31:0] sa, sb;
 sa = rrot(w[1],7)^rrot(w[1],18)^(w[1]>>3);
 sb = rrot(w[14],17)^rrot(w[14],19)^(w[14]>>10);
 wtnew = w[0] + sa + w[9] + sb;
endfunction


function logic [31:0] rrot(input logic[31:0] oldW, input logic[7:0] r);
begin	
	rrot = ((oldW << (32-r)) | (oldW>>r));
end
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

function logic [15:0] determine_num_blocks(input logic [31:0] size);
    if ((size << 3) % 512 <= 447)
      determine_num_blocks = ((size << 3)/512) + 1;
    else
      determine_num_blocks = ((size << 3)/512) + 2;
endfunction

  
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
				blk_counter <= 0; // increment blk_counter at the end of each M. Compare Num blks with blk_counter
				state <= PRO_READ;
				is_padding_block <= 0;
				is_exact_block <= 0;
				out_wc <= 0;
				num_blks <= determine_num_blocks(size);
				h0 <= 32'h6a09e667;
				h1 <= 32'hbb67ae85;
				h2 <= 32'h3c6ef372;
				h3 <= 32'ha54ff53a;
				h4 <= 32'h510e527f;
				h5 <= 32'h9b05688c;
				h6 <= 32'h1f83d9ab;
				h7 <= 32'h5be0cd19;
          end
		PRO_READ:
			// init A to H. BLK_COUNTER should already been pointing to the right addr
			begin
				$display("PRO_READ");
				$display("number of blk is %d",num_blks);
				t <= 0;
			
				A <= h0;
				B <= h1;
				C <= h2;
				D <= h3;
				E <= h4;
				F <= h5;
				G <= h6;
				H <= h7;
				// -----
				if ( (blk_counter == num_blks -1) && (is_exact_block == 1)) begin
					$display("filling last blk of exact blk");
					M[0] <= 32'h80000000;
					for (i=1; i<14; i=i+1) begin
						M[i] <= 32'h00000000;
					end
					M[14] <= size >> 29;
					M[15] <= size * 8;
					state <= PRE_COMPUTE_W;
					// TODO: block_counter
				end else if((blk_counter == num_blks -1) && (is_padding_block == 1)) begin
					// TODO: append in bits rather than bytes. check testbench
					$display("filling last blk of not exact blk");
					for (i=0; i<14; i=i+1) begin
						M[i] <= 32'h00000000;
					end
					M[14] <= size>>29;
					M[15] <= size * 8;
					state <= PRE_COMPUTE_W;
				end else begin
					for (i=0; i<16; i=i+1) begin
						M[i] <= 32'h00000000;
					end
					rc <= 1;
					wc <= 0;
					mem_we <= 0;
					mem_addr <= message_addr+blk_counter*16;
					state <= READ_ONLY;
				end
			end
		READ_ONLY:
			begin
				$display("read only");
				mem_we <= 0;
				mem_addr <= message_addr+blk_counter*16 + rc;
				rc <= rc + 1;
				state <= READ_AND_SET;
			end
		READ_AND_SET:
			begin
			$display("read and set, wc is %d", wc);
			if((blk_counter*64 + wc*4 >= size) ) begin	
				state <= PADDING;
			end
			else begin
				M[wc] <= mem_read_data;
				wc <= wc + 1;
				if(rc < 16) begin		
					mem_we <= 0;
					mem_addr <= message_addr+blk_counter*16 + rc;
					rc <= rc + 1;
					state <= READ_AND_SET;
				end else begin
					state <= SET_ONLY;
				end
			end
			end
		SET_ONLY:
			
			begin
				if((blk_counter*64 + wc*4 >= size) ) begin
					$display("set only and no more word to read");
					state <= PADDING;
				end else begin
				$display("set only but still have words left to read");
					M[wc] <= mem_read_data;
					// send to padding state if and only if no more message to read
					if((blk_counter*64 + (wc+1)*4 >= size)) begin
						state <= PADDING;
					end else begin
						state <= PRE_COMPUTE_W;
					end
				end
			end
		PADDING:
		if (size%64 == 0) begin
			$display("Find padding with exact blk");
			state <= PRE_COMPUTE_W;
			is_exact_block <= 1'b1;
		end
		else begin
			$display("padding");
			// TODO: must consider the edge case that exactly multiple of 64
			case ((size - blk_counter*64) % 4)
				0: M[(size-blk_counter*64)/4] <= 32'h80000000;
				1: M[(size-blk_counter*64)/4] <= M[(size-blk_counter*64)/4] & 32'h FF000000 | 32'h 00800000;
				2: M[(size-blk_counter*64)/4] <= M[(size-blk_counter*64)/4] & 32'h FFFF0000 | 32'h 00008000;
				3: M[(size-blk_counter*64)/4] <= M[(size-blk_counter*64)/4] & 32'h FFFFFF00 | 32'h 00000080;
			endcase
			
			if (blk_counter < num_blks - 1) begin
				// only need to add one extra bit to the tail and fill all with zero
				$display("it is a padding block");
				is_padding_block <= 1'b1;
				
			end else begin
				
				// add one extra bit to the tail and fill the last two words with size
				M[14] <= size >> 29;
				M[15] <= size * 8;
			end
			state <= PRE_COMPUTE_W;
		end
		PRE_COMPUTE_W:
			// only handle the first cycle.
			
			begin
				w[0] <= M[0];
				p <= H + sha256_k[0] + M[0];
				t <= t + 1;
				state <= COMPUTE_W;
			end
		COMPUTE_W:
		
		begin
			$display("COMPUTE_W state");
			if (t == 64) begin
				// done with this round, go back to proread
				{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[15], p);
				state <= UPDATE_HASH;
				
			end else begin
				// do on cycle of execution.
				if (t < 16) begin 
					{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[t-1], p);
					w[t] <= M[t];
					p <= G + sha256_k[t] + M[t];
				end else begin
					{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[15], p);
					for (i = 0; i < 15; i=i+1) begin
						w[i] <= w[i+1];
					end 
					w[15] <= wtnew();
					p <= G + sha256_k[t] +wtnew();
					//{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, wtnew(), t);
				end
				t <= t + 1;
				state <= COMPUTE_W;
				//state <= UPDATE_PARAM;
				//state <= UPDATE_S;
			end
		end
		/*
		UPDATE_S:
		begin
			$display("update_s");
			
			S0 <= rrot(A, 2) ^ rrot(A, 13) ^ rrot(A, 22);
			S1 <= rrot(E, 6) ^ rrot(E, 11) ^ rrot(E, 25);
			maj <= (A & B) ^ (A & C) ^ (B & C);
			ch <= (E & F) ^ ((~E) & G);
			state <= UPDATE_T;
		end
		
		UPDATE_T:
		begin
			$display("update_t");
			if(t < 16) begin
				t1 <= H + S1 + ch + sha256_k[t] + w[t];
			end else begin
				t1 <= H + S1 + ch + sha256_k[t] + w[15];
			end
			t2 <= S0 + maj;
			state <= UPDATE_PARAM;
		end
		
		UPDATE_PARAM:
		begin
			
			$display("update_param");
			
			if (t < 16) begin
				{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[t], t);
			end else begin
				{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[15], t);
			end
			t <= t + 1;
			state <= COMPUTE_W;
		end
		*/
		UPDATE_HASH:
			begin
				$display("UPDATE_HASH state");
				
				h0 <= h0 + A;
				h1 <= h1 + B;
				h2 <= h2 + C;
				h3 <= h3 + D;
				h4 <= h4 + E;
				h5 <= h5 + F;
				h6 <= h6 + G;
				h7 <= h7 + H;
				if (blk_counter >= num_blks - 1) begin
					state <= WRITE;
				end else begin
					blk_counter <= blk_counter + 1;
					state <= PRO_READ;
				end
			end
		
		WRITE:
		begin
			$display("write");
			mem_we <= 1;
         mem_addr <= output_addr + out_wc;
			case (out_wc)
			0: mem_write_data <= h0;
			1: mem_write_data <= h1;
			2: mem_write_data <= h2;
			3: mem_write_data <= h3;
			4: mem_write_data <= h4;
			5: mem_write_data <= h5;
			6: mem_write_data <= h6;
			7: mem_write_data <= h7;
			endcase
         //mem_write_data <= h[out_wc];
			 
			// write H0 to H7 into memory
			if(out_wc < 7) begin
				out_wc <= out_wc + 1;
				state <= WRITE;
			end else begin 
				state <= DONE;
			end
		end

		DONE:
		begin
			$display("DONE. Msg Size is %d",size);
			done <= 1;
			state <= IDLE;
		end
      endcase
  end

endmodule
